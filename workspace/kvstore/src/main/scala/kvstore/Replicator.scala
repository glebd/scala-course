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
  case class Dequeue(key: String, primary: ActorRef)

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
  
  var queueMap = Map.empty[String, Queue[Replicate]]
  
  var dequeueMsg: Cancellable = null
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }
  
  val retryTimeout = 50 milliseconds
  val dequeueTimeout = 50 milliseconds
  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    
    case r @ Replicate(key, valueOption, id) =>
      // add to key->queue map
      queueMap.get(key) match {
        case Some(q) =>
          q += r
        case None =>
          val q = Queue.empty[Replicate]
          q += r
          queueMap = queueMap + (key -> q)
      }
      log.debug(s"[Replicator] Received Replicate($key, $valueOption, $id) from $sender")
      dequeueMsg = context.system.scheduler.scheduleOnce(dequeueTimeout, self, Dequeue(key, sender))
      log.debug(s"[Replicator] Scheduling dequeue from $queueMap")
      
    case Dequeue(key, primary) =>
      if (queueMap.contains(key)) {
        val r = queueMap(key).dequeue
        val seq = nextSeq
        log.debug(s"[Replicator] Dequeued $r for k$key, queue: $queueMap")
        val reminder = context.system.scheduler.scheduleOnce(retryTimeout, self, Reminder(seq))
        acks = acks + (seq -> (primary, r, reminder))
        log.debug(s"[Replicator] Scheduled reminder $reminder in 100 ms")
      } else {
        log.error(s"[Replicator] Dequeue($key, $primary) and the queue is empty")
      }
      
    case SnapshotAck(key, seq) =>
      val (s, r, reminder) = acks(seq)
      log.debug(s"[Replicator] Received SnapshotAck($key, $seq, $reminder)")
      s ! Replicated(key, r.id)
      log.debug(s"[Replicator] Sending Replicated($key, ${r.id}) to $s")
      acks = acks - seq
      log.debug(s"[Replicator] Removed seq $seq from acks, now $acks")
      if (queueMap.contains(key)) {
        if (!queueMap(key).isEmpty) {
          dequeueMsg = context.system.scheduler.scheduleOnce(dequeueTimeout, self, Dequeue(key, s))
          log.debug(s"[Replicator] Scheduling dequeue for k$key from $queueMap")
        } else {
          log.debug("[Replicator] Queue for k$key is empty")
        }
      }
      
    case Reminder(seq) =>
      log.debug(s"[Replicator] Received Reminder($seq)")
      if (acks contains seq) {
        val (s, r, reminder) = acks(seq)
        log.debug(s"[Replicator] Re-sending Snapshot(${r.key}, ${r.valueOption}, $seq)")
        replica ! Snapshot(r.key, r.valueOption, seq)
        val reminder1 = context.system.scheduler.scheduleOnce(retryTimeout, self, Reminder(seq))
        acks = acks + (seq -> (s, r, reminder1))
        log.debug(s"[Replicator] Scheduled reminder $reminder1 in 100 ms")
      } else {
        log.error(s"[Replicator] No such seq $seq in acks!")
      }
      
  }

  override def postStop() = {
    log.debug("[Replicator] postStop()")
    acks foreach (_._2._3.cancel())
    if (dequeueMsg != null) dequeueMsg.cancel()
  }

}
