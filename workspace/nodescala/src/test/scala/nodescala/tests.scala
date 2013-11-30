package nodescala

import scala.language.postfixOps
import scala.util.{ Try, Success, Failure }
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{ async, await }
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("A Future should be delayed") {
    @volatile var test = 0
    val delayed = Future.delay(1 second)
    delayed onComplete { case _ => test = 42 }
    Await.ready(delayed, 3 seconds)
    Thread.sleep(10)
    assert(test === 42)
  }

  test("All Futures should complete") {
    val f1 = Future.always(1)
    val f2 = Future.always(2)
    val f3 = Future.always(3)

    val fs = List(f1, f2, f3)
    val all = Future.all(fs)
    assert(Await.result(all, 1 second) === List(1, 2, 3))
  }

  test("All Futures should fail if one fails") {
    val f1 = Future.always(1)
    val f2 = Future.always(2)
    val f3 = Future.failed(new Exception)

    val fs = List(f1, f2, f3)
    val all = Future.all(fs)
    Await.ready(all, 1 second)
    all.value match {
      case Some(Success(v)) => assert(false)
      case Some(Failure(e)) =>
    }
  }

  // https://class.coursera.org/reactive-001/forum/thread?thread_id=1177#post-4694
  test("Future.any") {
    val never = Future.never
    val bad = Future { throw new Exception }
    var sawExceptions = false
    try {
      Await.result(Future.any(List(never, bad)), 1 second)
      sawExceptions = true
    } catch {
      case t: TimeoutException =>
      case e: Exception => sawExceptions = true
    }
    assert(sawExceptions, "Exceptions not seen.")
  }

  // https://class.coursera.org/reactive-001/forum/thread?thread_id=1150#post-4675
  test("all ok") {
    val f1 = Future.always(123)
    val f2 = Future.always(456)
    val fs = List(f1, f2)
    val all = Future.all(fs)
    val r = Await.result(all, 1 seconds)
    assert(r equals List(123, 456))
  }

  test("any ok") {
    val f1 = Future.always(123)
    val f2 = Future.always(456)
    val fs = List(f1, f2)
    val any = Future.any(fs)
    val r = Await.result(any, 1 seconds)
    assert(r == 123 || r == 456)
  }

  // https://class.coursera.org/reactive-001/forum/thread?thread_id=1190#post-4742
  test("any should complete with the value of first to complete") {
    val p1 = Promise[Int]
    val p2 = Promise[Int]

    val all = Future.any(List(p1.future, p2.future))
    p2.success(2)
    p1.success(1)

    assert(Await.result(all, 1 second) == 2)
  }

  test("any should complete with failure if first to complete fails") {
    val p1 = Promise[Int]
    val p2 = Promise[Int]

    val all = Future.any(List(p1.future, p2.future))
    p2.failure(new Throwable("Failure"))
    p1.success(1)

    assert(intercept[Throwable] {
      Await.result(all, 1 second)
    }.getMessage() == "Failure")
  }
  
  test("A Future should be completed with all the results") {
    val is = (1 to 10).toList 
    val ifs = is map { Future always _ }
    val fis = Future all ifs
    val is2 = Await.result(fis, 1 second)
    assert(is == is2)
  }

  test("Test `now`") {
    // completed: `now` returns value
    assert(1 == Future.always(1).now)

    // not completed: `now` throws NoSuchElementException
    try {
      Future.never.now
      assert(false)
    } catch {
      case t: NoSuchElementException => // OK!
    }

    // completed with error: `now` throws the error
    val p = Promise[Int]
    p.failure(new Throwable("Failure"))
    assert(intercept[Throwable] {
      p.future.now
    }.getMessage() == "Failure")
  }

  test("`continue` should handle exception") {
    val f = future(throw new Throwable("Error"))
    val s = f.continue(_ => "Success")
    assert(Await.result(s, 100 milliseconds) === "Success")
  }

  test("test continueWith should handle exception thrown beforehand") {
    @volatile var test = "";
    def sss = Future[String] { Thread.sleep(10); throw new Exception("first explosion") }
    def ccont(fff: Future[String]): Int = fff.value match {
      case Some(Success(se)) => -333;
      case Some(Failure(e)) => { test = e.getMessage(); -666 }
      case None => -999
    }
    val nnn: Future[Int] = sss.continueWith(ccont)
    Await.ready(nnn, 1 seconds)
    assert(test === "first explosion")
  }

  test("test continueWith should handle exception thrown by continuing function") {
    @volatile var test = "";
    def sss = Future[String] { Thread.sleep(100); throw new Exception("first explosion") }
    def ccont(ttt: Future[String]): Int = ttt match {
      case _ => throw new Exception("second explosion")
    }
    val nnn: Future[Int] = sss.continueWith(ccont)
    nnn onComplete {
      case Failure(e) => test = e.getMessage
      case Success(t) => test = "nope"
    }
    Await.ready(nnn, 1 seconds)
    Thread.sleep(10) // allow time for callback to be called
    assert(test === "second explosion")
  }

  test("A future should be continued") {
    val result = Future[Int] {
      1
    } continueWith {
      f => 2
    }
    assert(Await.result(result, 1 second) === 2)
  }

  test("A Future should be continued 2") {
    val result = Future[String] {
      throw new IllegalStateException()
    }.continueWith { f =>
      "continued"
    }
    assert(Await.result(result, 1 second) == "continued")
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  // https://class.coursera.org/reactive-001/forum/thread?thread_id=1282
  test("run") {
    var finished = false
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
//          println("working")
          Thread.sleep(100)
        }
//        println("done") 
        finished = true
      }
    }
    Future.delay(1 seconds) onComplete {
      case _ => working.unsubscribe()
    }
    Await.ready(Future.delay(2 seconds), Duration.Inf)
    assert(finished) // todo: this fails. why?
  }

  // https://class.coursera.org/reactive-001/forum/thread?thread_id=1024#post-4202
  test("test Future.run") {
    import org.scalatest.concurrent.AsyncAssertions._
    import org.scalatest.time._

    val w = new Waiter

    val working = Future.run() { ct =>
      async {
        while (ct.nonCancelled) {
//          println("working")
          Thread.sleep(200)
        }
//        println("done")
        w { assert(true) }
        w.dismiss()
      }
    }

    Future.delay(1 seconds) onComplete {
      case _ => working.unsubscribe()
    }

    w.await(Timeout(Span(2, Seconds)))
  }

  test("test Future.run with Promise") {

    val p = Promise[Boolean]()

    val working = Future.run() {
      ct =>
        async {
          while (ct.nonCancelled) {
//            println("working")
            Thread.sleep(200)
          }
//          println("done")
          p.success(true)
        }
    }

    Future.delay(1 seconds) onComplete {
      case _ => working.unsubscribe()
    }

    assert(Await.result(p.future, 2 second))
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

  test("Server should cancel a infinite response") {
    val random = new scala.util.Random()
    val endlessReply = (_: Request) => random.alphanumeric.map(_.toString).iterator
    val server = new DummyServer(8191)
    val subscription = server.start("/testDir")(endlessReply)
    Thread.sleep(500) // wait until server is accepting connections
    val webpage: DummyExchange = server.emit("/testDir", Map())
    Thread.sleep(10) // wait until something is written to exchange.
    subscription.unsubscribe()
    Await.ready(webpage.loaded.future, 1 second)

    // listener must've been stopped as well,
    // i.e. should refuse to create new context.
    intercept[TestFailedException] {
      server.listeners.head._2.createContext(_ => ())
    }
  }
}
