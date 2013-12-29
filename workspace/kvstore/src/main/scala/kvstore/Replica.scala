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
  
  var retries = Map.empty[Long, Cancellable]
  var timeouts = Map.empty[Long, Cancellable]
  var persistAcks = Map.empty[Long, (ActorRef, Long, Boolean)] // seq -> (sender, id, received)
  var replicationAcks = Map.empty[Long, (ActorRef, ActorRef, String, Long, Boolean)] // seq -> (replica, client, key, id, received)
  
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
      val allSec = (replicas - self)
      val joiningSec = allSec.diff(secondaries.keySet)
      val leavingSec = secondaries.keySet.diff(allSec)
      // stop replicators for leaving replicas
      val replicatorsToStop = secondaries.filterKeys(x => leavingSec.contains(x)).values.toSet
      replicators = replicators -- replicatorsToStop
      replicatorsToStop foreach { context.stop(_) }
      // remove leaving replicas from secondaries
      secondaries = secondaries -- leavingSec
      // stop waiting for any replication ACKs from leaving replicas
      // send ACKs for pending replications for leaving replicas
      replicationAcks foreach {
        case (seq, (replica, client, key, id, rcvd)) if leavingSec.contains(replica) =>
          context.stop(replica)
          // stop any pending retries for leaving replicas
          if (retries.contains(seq)) {
            retries(seq).cancel()
            retries = retries - seq
          }
          self ! SnapshotAck(key, seq)
      }
      // add new replicas to secondaries along with their new replicators
      secondaries = secondaries ++ joiningSec.zipWithIndex.map {
        case (sec, i) => (sec, context.actorOf(Replicator.props(sec)))
      }.toMap
      // start replicating latest changes to new replicas (no ACKS needed)
      kv foreach {
        case (k, v) =>
          joiningSec foreach {
            sec => sec ! Snapshot(k, Some(v), 0L)
            // TODO: retry initial replication?
          }
      }
    
    case Insert(key, value, id) =>
      log.debug(s"received Insert($key, $value, $id)")
      kv = kv + (key -> value)
      persistence ! Persist(key, Some(value), sequence)
      persistAcks = persistAcks + (sequence -> (sender, id, false))
      val retry = context.system.scheduler.scheduleOnce(100 milliseconds, self, RetryPersist(key, Some(value), sequence))
      retries = retries + (sequence -> retry)
      val timeout = context.system.scheduler.scheduleOnce(1 second, self, OperationTimeout(sequence))
      timeouts = timeouts + (sequence -> timeout)
      secondaries foreach {
        case (replica, replicator) =>
          log.debug(s"Sending Snapshot($key, Some($value), $sequence) to $replica")
          replica ! Snapshot(key, Some(value), sequence)
          replicationAcks = replicationAcks + (sequence -> (replica, sender, key, id, false))
      }
      sequence = sequence + 1
      
    case Remove(key, id) =>
      if (kv.contains(key))
        kv = kv - key
      persistence ! Persist(key, None, sequence)
      persistAcks = persistAcks + (sequence -> (sender, id, false))
      val retry = context.system.scheduler.scheduleOnce(100 milliseconds, self, RetryPersist(key, None, sequence))
      retries = retries + (sequence -> retry)
      val timeout = context.system.scheduler.scheduleOnce(1 second, self, OperationTimeout(sequence))
      timeouts = timeouts + (sequence -> timeout)
      secondaries foreach {
        case (replica, replicator) =>
          replica ! Snapshot(key, None, sequence)
          replicationAcks = replicationAcks + (sequence -> (replica, sender, key, id, false))
      }
      sequence = sequence + 1
      
    case Persisted(key, seq) =>
      log.debug(s"Received Persisted($key, $seq)")
      if (retries.contains(seq)) {
        retries(seq).cancel()
        retries = retries - seq
      }
      if (persistAcks.contains(seq)) {
        val (pclient, pid, _) = persistAcks(seq)
        if (replicationAcks.contains(seq)) {
          val (replica, rclient, key, rid, rcvd) = replicationAcks(seq)
          if (rcvd) {
            log.debug(s"Sending OperationAck($pid) to $pclient because SnapshotAck was received for it earlier")
            pclient ! OperationAck(pid)
            persistAcks = persistAcks - seq
            replicationAcks = replicationAcks - seq
          } else {
            persistAcks = persistAcks + (seq -> (pclient, pid, true))
          }
        } else {
          log.debug(s"Sending OperationAck($pid) to $pclient because not replicating")
          pclient ! OperationAck(pid)
          persistAcks = persistAcks - seq
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
      
    case OperationTimeout(seq) =>
      if (timeouts.contains(seq)) {
        timeouts(seq).cancel()
        timeouts = timeouts - seq
      }
      if (persistAcks.contains(seq)) {
        val (client, id, _) = persistAcks(seq)
        persistAcks = persistAcks - seq
        client ! OperationFailed(id)
      } else if (replicationAcks.contains(seq)) {
        val (replica, client, key, id, _) = replicationAcks(seq)
        replicationAcks = replicationAcks - seq
        client ! OperationFailed(id)
      }
      
    case SnapshotAck(key, seq) =>
      log.debug(s"Received SnapshotAck($key, $seq)")
      if (replicationAcks.contains(seq)) {
        val (replica, rclient, rkey, rid, _) = replicationAcks(seq)
        if (persistAcks.contains(seq)) {
          val (pclient, pid, rcvd) = persistAcks(seq)
          if (rcvd) {
            log.debug(s"Sending OperationAck($rid) to $rclient because Persisted was received for it earlier")
            rclient ! OperationAck(rid)
            persistAcks = persistAcks - seq
            replicationAcks = replicationAcks - seq
          } else {
            replicationAcks = replicationAcks + (seq -> (replica, rclient, rkey, rid, true))
          }
        } else {
          log.debug(s"Sending OperationAck($rid) to $rclient because not persisting")
          rclient ! OperationAck(rid)
          replicationAcks = replicationAcks - seq
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
