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
  case class Dequeue()

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor with ActorLogging {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // map of reminders
  var reminders = Map.empty[Long, Cancellable]
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
  
  def safeCancelAndRemove(m: Map[Long, Cancellable], seq: Long): Map[Long, Cancellable] = {
    if (m.contains(seq)) {
      m(seq).cancel
      m - seq
    } else m
  }
  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    
    case r @ Replicate(key, valueOption, id) =>
      queue += r
      log.debug(s"Received Replicate($key, $valueOption, $id) from $sender")
      dequeueMsg = context.system.scheduler.scheduleOnce(1 millisecond, self, Dequeue())
      log.debug(s"Scheduling dequeue from $queue")
      
    case Dequeue() =>
      val r = queue.dequeue
      val seq = nextSeq
      log.debug(s"Dequeued $r, queue: $queue")
      acks = acks + (seq -> (sender, r))
      replica ! Snapshot(r.key, r.valueOption, seq)
      log.debug(s"Sending Snapshot($r.key, $r.valueOption, $seq) to replica $replica")
      val remind = context.system.scheduler.scheduleOnce(100 milliseconds, self, Reminder(seq))
      reminders = reminders + (seq -> remind)
      log.debug(s"Scheduling reminder, reminders: $reminders")
      
    case SnapshotAck(key, seq) =>      
      reminders = safeCancelAndRemove(reminders, seq)
      val (s, r) = acks(seq)
      log.debug(s"Received SnapshotAck($key, $seq), reminders: $reminders")
      s ! Replicated(key, r.id)
      log.debug(s"Sending Replicated($key, $r.id) to $s")
      acks = acks - seq
      log.debug(s"Removed seq $seq from acks, now $acks")
      if (!queue.isEmpty) {
        dequeueMsg = context.system.scheduler.scheduleOnce(1 millisecond, self, Dequeue())
        log.debug(s"Scheduling dequeue from $queue")
      } else {
        log.debug("Queue empty")
      }
      
    case Reminder(seq) =>
      if (reminders.contains(seq)) {
        if (acks contains seq) {
          reminders = safeCancelAndRemove(reminders, seq)
          log.debug(s"Received and removed Reminder($seq), reminders now: $reminders")
          val (s, r) = acks(seq)
          log.debug(s"Re-sending Snapshot($r.key, $r.valueOption, $seq)")
          replica ! Snapshot(r.key, r.valueOption, seq)
          val remind = context.system.scheduler.scheduleOnce(100 milliseconds, self, Reminder(seq))
          reminders = reminders + (seq -> remind)
          log.debug(s"Reminders: $reminders")
        } else {
          log.error(s"No such seq $seq in acks!")
        }
      } else {
        log.warning(s"Received Reminder($seq) not found")
      }
      
  }

  override def postStop() = {
    log.debug("Replicator postStop()")
    reminders foreach (_._2.cancel())
    dequeueMsg.cancel()
  }

}
