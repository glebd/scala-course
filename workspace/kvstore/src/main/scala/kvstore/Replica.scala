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

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation
  
  case class RetryPersist(key: String, valueOption: Option[String], id: Long) extends Operation

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
  
  val persistence = context.actorOf(persistenceProps)
  context.watch(persistence)

  arbiter ! Join
  
  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }
  
  val common: Receive = {
    case Get(key, id) =>
      sender ! GetResult(key, kv.get(key), id)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = common orElse /*LoggingReceive*/ {

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
      replicatorsToStop foreach { context.stop(_) }
      replicators = replicators -- replicatorsToStop
      log.debug(s"Replicators without leaving replicators: $replicators")
      // remove leaving replicas from secondaries
      secondaries = secondaries -- leavingSec
      log.debug(s"Secondaries without leaving replicas: $secondaries")
      // stop waiting for any replication ACKs from leaving replicas
      // send ACKs for pending replications from leaving replicas
      acks foreach {
        case (id, (client, key, persisted, reps)) =>
          reps foreach { case rep =>
            if (leavingSec.contains(rep)) {
              val replicator = secondaries(rep)
              self.tell(Replicated(key, id), replicator) // spoof message from replicator
              log.debug(s"Sending Replicated($key, $id) to self as replicator $replicator and stopping replicator")
              context.stop(replicator)
            }
          }
      }
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
            log.debug(s"Sending initial Replicate($k, Some($v), sequence) to replicator $replicator")
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
      
    case RetryPersist(key, valueOption, seq) =>
      if (retries.contains(seq)) {
        retries(seq).cancel()
        retries = retries - seq
      }
      persistence ! Persist(key, valueOption, seq)
      val retry = context.system.scheduler.scheduleOnce(100 milliseconds, self, RetryPersist(key, valueOption, seq))
      retries = retries + (seq -> retry)
      
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
        log.debug(s"Acks contains id $id")
        val (client, key, persisted, reps) = acks(id)
        // check if the replica of the sender replicator is in the set of replicas pending ACKs
        val replica = secondaries.find(_._2 == sender)
        val reps1 = reps - replica.get._2
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
  val replica: Receive = common orElse /*LoggingReceive*/ {
    
    case Snapshot(key, valueOption, seq) if seq > sequence =>
      
    case Snapshot(key, valueOption, seq) if seq < sequence =>
      sender ! SnapshotAck(key, seq)
      
    case Snapshot(key, valueOption, seq) =>
      replicators = replicators + sender
      sequence += 1
      valueOption match {
        case None => kv = kv - key
        case Some(value) => kv = kv + (key -> value)
      }
      persistence ! Persist(key, valueOption, seq)
      val retry = context.system.scheduler.scheduleOnce(100 milliseconds, self, RetryPersist(key, valueOption, seq))
      retries = retries + (seq -> retry)
      
    case Persisted(key, seq) =>
      replicators.head ! SnapshotAck(key, seq)
      sequence = math.max(sequence, seq + 1)
      if (retries.contains(seq)) {
        retries(seq).cancel()
        retries = retries - seq
      }
      
    case RetryPersist(key, valueOption, seq) =>
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
