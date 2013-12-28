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
  var acks = Map.empty[Long, (ActorRef, Long)] // seq -> (sender, id)
  
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
  val leader: Receive = common orElse {
    
    case Insert(key, value, id) =>
      kv = kv + (key -> value)
      persistence ! Persist(key, Some(value), sequence)
      acks = acks + (sequence -> (sender, id))
      val retry = context.system.scheduler.scheduleOnce(100 milliseconds, self, RetryPersist(key, Some(value), sequence))
      retries = retries + (sequence -> retry)
      sequence = sequence + 1
      
    case Remove(key, id) =>
      if (kv.contains(key))
        kv = kv - key
      persistence ! Persist(key, None, sequence)
      acks = acks + (sequence -> (sender, id))
      val retry = context.system.scheduler.scheduleOnce(100 milliseconds, self, RetryPersist(key, None, sequence))
      retries = retries + (sequence -> retry)
      sequence = sequence + 1
      
    case Persisted(key, seq) =>
      if (retries.contains(seq)) {
        retries(seq).cancel()
        retries = retries - seq
      }
      if (acks.contains(seq)) {
        val (client, id) = acks(seq)
        client ! OperationAck(id)
        acks = acks - seq
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
  }

}
