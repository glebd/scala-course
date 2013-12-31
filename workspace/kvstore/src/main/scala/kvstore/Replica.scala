package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import akka.event.Logging
import akka.event.LoggingReceive
import akka.actor.ActorLogging
import scala.language.postfixOps
import akka.actor.Cancellable
import akka.actor.OneForOneStrategy
import akka.dispatch.sysmsg.Resume

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation
  
  case class RetryPersist(key: String, valueOption: Option[String], id: Long) extends Operation
  case class OperationTimeout(id: Long)

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor with ActorLogging {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  
  var retries = Map.empty[Long, Cancellable] // id -> retry (persistence)
  var timeouts = Map.empty[Long, Cancellable] // id -> timeout (persistence)
  var acks = Map.empty[Long, (ActorRef, String, Boolean, Set[ActorRef])] // id -> (client, key, persisted, Set(replica))
  
  var sequence = 0L
  
  var persistence = context.actorOf(persistenceProps, "persistence")
  context.watch(persistence)

  arbiter ! Join
  
  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 100) {
    case _: PersistenceException =>
      log.debug("Restarting strategy due to persistence exception")
      Restart
    case e: NoSuchElementException =>
      log.debug(s"Restarting strategy due to key not found exception; self = $self")
      Restart
  }
  
  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }
  
  val common: Receive = LoggingReceive {
    
    case Get(key, id) =>
      sender ! GetResult(key, kv.get(key), id)
      
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = common orElse LoggingReceive {

    case Replicas(replicas) =>
      log.debug(s"Received replicas: $replicas")
      val allSec = (replicas - self)
      log.debug(s"All secondaries: $allSec")
      val joiningSec = allSec.diff(secondaries.keySet)
      log.debug(s"Joining secondaries: $joiningSec")
      val leavingSec = secondaries.keySet.diff(allSec)
      log.debug(s"Leaving secondaries: $leavingSec")
      // stop replicators for leaving replicas
      val replicatorsToStop = secondaries.filterKeys(x => leavingSec.contains(x)).values.toSet
      log.debug(s"Stopping replicators: $replicatorsToStop")
      replicatorsToStop foreach { _ ! PoisonPill }
      replicators = replicators -- replicatorsToStop
      log.debug(s"Replicators without leaving replicators: $replicators")
      // stop waiting for any replication ACKs from leaving replicas
      // send ACKs for pending replications from leaving replicas
      var idsToAck = Set.empty[Long]
      acks foreach {
        case (id, (client, key, persisted, reps)) =>
          reps foreach { case rep =>
            if (leavingSec.contains(rep)) {
              client ! OperationAck(id)
              log.debug(s"Sending OperationAck($id) to client $client")
              idsToAck = idsToAck + id
            }
          }
      }
      log.debug(s"Removing acked operations from acks: $idsToAck")
      acks = acks -- idsToAck
      log.debug(s"Acks without leaving/acked replicas: $acks")
      // remove leaving replicas from secondaries
      secondaries = secondaries -- leavingSec
      log.debug(s"Secondaries without leaving replicas: $secondaries")
      // add new replicas to secondaries along with their new replicators
      secondaries = secondaries ++ joiningSec.zipWithIndex.map {
        case (sec, i) => (sec, context.actorOf(Replicator.props(sec)))
      }.toMap
      log.debug(s"Secondaries with joining replicas: $secondaries")
      // start replicating latest changes to new replicators
      kv foreach {
        case (k, v) =>
          joiningSec foreach { replica =>
            val replicator = secondaries(replica)
            log.debug(s"Sending initial Replicate($k, Some($v), $sequence) to replicator $replicator")
            replicator ! Replicate(k, Some(v), sequence)
            sequence = sequence + 1
            // TODO: retry initial replication?
          }
      }
    
    case Insert(key, value, id) =>
      log.debug(s"Received Insert($key, $value, $id) from $sender")
      kv = kv + (key -> value)
      persistence ! Persist(key, Some(value), id)
      val retry = context.system.scheduler.scheduleOnce(100 milliseconds, self, RetryPersist(key, Some(value), id))
      retries = retries + (id -> retry)
      val timeout = context.system.scheduler.scheduleOnce(1 second, self, OperationTimeout(id))
      timeouts = timeouts + (id -> timeout)
      var reps = Set.empty[ActorRef]
      secondaries foreach {
        case (replica, replicator) =>
          log.debug(s"Sending Replicate($key, Some($value), $id) to $replicator")
          replicator ! Replicate(key, Some(value), id)
          reps = reps + replica
      }
      acks = acks + (id -> (sender, key, false, reps))
      
    case Remove(key, id) =>
      log.debug(s"Received Remove($key, $id) from $sender")
      if (kv.contains(key))
        kv = kv - key
      persistence ! Persist(key, None, id)
      val retry = context.system.scheduler.scheduleOnce(100 milliseconds, self, RetryPersist(key, None, id))
      retries = retries + (id -> retry)
      val timeout = context.system.scheduler.scheduleOnce(1 second, self, OperationTimeout(id))
      timeouts = timeouts + (id -> timeout)
      var reps = Set.empty[ActorRef]
      secondaries foreach {
        case (replica, replicator) =>
          log.debug(s"Sending Replicate($key, None, $id) to $replicator")
          replicator ! Replicate(key, None, id)
          reps = reps + replica
      }
      acks = acks + (id -> (sender, key, false, reps))
      
    case Persisted(key, id) =>
      log.debug(s"Received Persisted($key, $id)")
      if (retries.contains(id)) {
        retries(id).cancel()
        retries = retries - id
        log.debug(s"Cancelling persistence retry for $id, retries = $retries")
      }
      if (acks.contains(id)) {
        val (client, _, _, reps) = acks(id)
        log.debug(s"Acks has id $id with reps = $reps")
        // check if there are any replicas with pending acks for this id
        if (reps.isEmpty) {
          // no pending acks from replicas, can send success to client
          log.debug(s"Sending OperationAck($id) to $client because persisted=true and there are no pending ACKs from replicas")
          client ! OperationAck(id)
          acks = acks - id
          if (timeouts.contains(id)) {
            timeouts(id).cancel()
            timeouts = timeouts - id
            log.debug(s"Cancelling timeout for id $id, timeouts = $timeouts")
          }
        } else {
          // mark the corresponding ack as persisted
          acks = acks + (id -> (client, key, true, reps))
          log.debug(s"Adding pending ACK for id $id to acks: $acks")
        }
      }
      
    case RetryPersist(key, valueOption, id) =>
      if (retries.contains(id)) {
        retries(id).cancel()
        retries = retries - id
      }
      persistence ! Persist(key, valueOption, id)
      val retry = context.system.scheduler.scheduleOnce(100 milliseconds, self, RetryPersist(key, valueOption, id))
      retries = retries + (id -> retry)
      
    case OperationTimeout(id) =>
      log.debug(s"Received OperationTimeout($id)")
      if (timeouts.contains(id)) {
        timeouts(id).cancel()
        timeouts = timeouts - id
        log.debug(s"Cancelling timeout for id $id, timeouts = $timeouts")
      }
      if (acks.contains(id)) {
        val (client, key, persisted, reps) = acks(id)
        acks = acks - id
        client ! OperationFailed(id)
        log.debug(s"Sending OperationFailed($id) to $client, acks = $acks")
      }
      
    case Replicated(key, id) =>
      // sender = Replicator
      log.debug(s"Received Replicated($key, $id) from $sender")
      if (acks.contains(id)) {
        val (client, key, persisted, reps) = acks(id)
        log.debug(s"Acks contains id $id with reps = $reps, persisted = $persisted")
        // check if the replica of the sender replicator is in the set of replicas pending ACKs
        val replica = secondaries.find(_._2 == sender)
        log.debug(s"Found replica $replica for sender replicator $sender")
        val reps1 = reps - replica.get._1
        log.debug(s"New reps = $reps1")
        if (reps1.isEmpty && persisted) {
          // persisted and no pending ACKs from replicas, report success
          log.debug(s"Sending OperationAck($id) to $client because there are no pending ACKs from replicas and persisted=true")
          client ! OperationAck(id)
          acks = acks - id
          if (timeouts.contains(id)) {
            timeouts(id).cancel()
            timeouts = timeouts - id
            log.debug(s"Cancelling timeout for id $id, timeouts = $timeouts")
          }
        } else {
          // update set of pending ACKs from replicas
          acks = acks + (id -> (client, key, persisted, reps1))
          log.debug(s"Updating pending ACK for id $id with reps: $reps1")
        }
      }
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = common orElse LoggingReceive {
    
    case Snapshot(key, valueOption, seq) if seq > sequence =>
      log.debug(s"[Replica] Snapshot($key, $valueOption, $seq) and $seq > $sequence")
      replicators = replicators + sender
      log.debug(s"[Replica] Replicators: $replicators")
      if (retries.contains(seq)) {
        retries(seq).cancel()
        retries = retries - seq
      }
      
    case Snapshot(key, valueOption, seq) if seq < sequence =>
      log.debug(s"[Replica] Snapshot($key, $valueOption, $seq) and $seq < $sequence")
      replicators = replicators + sender
      log.debug(s"[Replica] Replicators: $replicators")
      sender ! SnapshotAck(key, seq)
      log.debug(s"[Replica] Sending SnapshotAck($key, $seq) to $sender")
      if (retries.contains(seq)) {
        retries(seq).cancel()
        retries = retries - seq
      }
      
    case Snapshot(key, valueOption, seq) =>
      log.debug(s"[Replica] Snapshot($key, $valueOption, $seq) and $seq == $sequence")
      replicators = replicators + sender
      log.debug(s"[Replica] Replicators: $replicators")
      if (retries.contains(seq)) {
        retries(seq).cancel()
        retries = retries - seq
      }
      sequence = math.max(sequence, seq + 1)
      valueOption match {
        case None => kv = kv - key
        case Some(value) => kv = kv + (key -> value)
      }
      persistence ! Persist(key, valueOption, seq)
      val retry = context.system.scheduler.scheduleOnce(100 milliseconds, self, RetryPersist(key, valueOption, seq))
      retries = retries + (seq -> retry)
      
    case Persisted(key, seq) =>
      log.debug(s"[Replica] Persisted($key, $seq)")
      replicators.head ! SnapshotAck(key, seq)
      log.debug(s"[Replica] Sending SnapshotAck($key, $seq) to ${replicators.head}")
      sequence = math.max(sequence, seq + 1)
      log.debug(s"[Replica] New sequence = $sequence")
      if (retries.contains(seq)) {
        retries(seq).cancel()
        retries = retries - seq
      }
      
    case RetryPersist(key, valueOption, seq) =>
      log.debug(s"[Replica] RetryPersist($key, $valueOption, $seq)")
      if (retries.contains(seq)) {
        retries(seq).cancel()
        retries = retries - seq
      }
      persistence ! Persist(key, valueOption, seq)
      val retry = context.system.scheduler.scheduleOnce(100 milliseconds, self, RetryPersist(key, valueOption, seq))
      retries = retries + (seq -> retry)
  }
  
  override def postStop() = {
    retries foreach (_._2.cancel())
    timeouts foreach (_._2.cancel())
  }

}
