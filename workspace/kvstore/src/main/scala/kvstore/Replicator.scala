package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Cancellable
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.collection.mutable.Queue
import akka.actor.ActorLogging

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)
  
  case class Reminder(seq: Long)
  case class Dequeue(primary: ActorRef)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor with ActorLogging {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender, request and reminder Cancellable
  var acks = Map.empty[Long, (ActorRef, Replicate, Cancellable)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  val queue = Queue.empty[Replicate]
  
  var dequeueMsg: Cancellable = null
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }
  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    
    case r @ Replicate(key, valueOption, id) =>
      queue += r
      log.debug(s"Received Replicate($key, $valueOption, $id) from $sender")
      dequeueMsg = context.system.scheduler.scheduleOnce(1 millisecond, self, Dequeue(sender))
      log.debug(s"Scheduling dequeue from $queue")
      
    case Dequeue(primary) =>
      val r = queue.dequeue
      val seq = nextSeq
      log.debug(s"Dequeued $r, queue: $queue")
      val reminder = context.system.scheduler.scheduleOnce(1 millisecond, self, Reminder(seq))
      acks = acks + (seq -> (primary, r, reminder))
      log.debug(s"Scheduled reminder $reminder in 1 ms")
      
    case SnapshotAck(key, seq) =>
      val (s, r, reminder) = acks(seq)
      log.debug(s"Received SnapshotAck($key, $seq, $reminder)")
      s ! Replicated(key, r.id)
      log.debug(s"Sending Replicated($key, ${r.id}) to $s")
      acks = acks - seq
      log.debug(s"Removed seq $seq from acks, now $acks")
      if (!queue.isEmpty) {
        dequeueMsg = context.system.scheduler.scheduleOnce(1 millisecond, self, Dequeue(s))
        log.debug(s"Scheduling dequeue from $queue")
      } else {
        log.debug("Queue empty")
      }
      
    case Reminder(seq) =>
      log.debug(s"Received Reminder($seq)")
      if (acks contains seq) {
        val (s, r, reminder) = acks(seq)
        log.debug(s"Re-sending Snapshot(${r.key}, ${r.valueOption}, $seq)")
        replica ! Snapshot(r.key, r.valueOption, seq)
        val reminder1 = context.system.scheduler.scheduleOnce(100 milliseconds, self, Reminder(seq))
        acks = acks + (seq -> (s, r, reminder1))
        log.debug(s"Scheduled reminder $reminder1 in 100 ms")
      } else {
        log.error(s"No such seq $seq in acks!")
      }
      
  }

  override def postStop() = {
    log.debug("Replicator postStop()")
    acks foreach (_._2._3.cancel())
    dequeueMsg.cancel()
  }

}
