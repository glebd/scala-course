/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case msg => root ! msg
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = ???

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    
    case msg @ Contains(requester, id, e) =>
      if (e < elem) {
        if (subtrees.contains(Left)) subtrees(Left) ! msg
        else requester ! ContainsResult(id, false)
      } else if (e > elem) {
        if (subtrees.contains(Right)) subtrees(Right) ! msg
        else requester ! ContainsResult(id, false)
      } else {
        requester ! ContainsResult(id, !removed)
      }
      
    case msg @ Insert(requester, id, e) =>
      if (e < elem) { // less than current element, belongs to the left subtree
        subtrees.get(Left) match {
          case Some(subtree) => subtree ! msg // found a left subtree, let it handle the message
          case None => // add a left subtree as a node with the requested value
            subtrees = subtrees + (Left -> context.actorOf(BinaryTreeNode.props(e, initiallyRemoved = false)))
            requester ! OperationFinished(id)
        }
      } else if (e > elem) { // more than current element, belongs to the right subtree
        subtrees.get(Right) match {
          case Some(subtree) => subtree ! msg // found a right subtree, let it handle the message
          case None => // add a right subtree as a node with the requested value
            subtrees = subtrees + (Right -> context.actorOf(BinaryTreeNode.props(e, initiallyRemoved = false)))
            requester ! OperationFinished(id)
        }
      } else {
        // same as current element
        if (removed) removed = false
        requester ! OperationFinished(id)
      }
    
    case msg @ Remove(requester, id, e) =>
      if (e < elem) {
        subtrees.get(Left) match {
          case Some(subtree) => subtree ! msg
          case None => //throw new NoSuchElementException
        }
      } else if (e > elem) {
        subtrees.get(Right) match {
          case Some(subtree) => subtree ! msg
          case None => //throw new NoSuchElementException
        }
      } else {
        removed = true
        requester ! OperationFinished(id)
      }
      
    case _ =>
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???

}
