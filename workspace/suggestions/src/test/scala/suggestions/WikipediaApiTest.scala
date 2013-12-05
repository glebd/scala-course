package suggestions

import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true)
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("recovered") {
    val e = new Exception()
    val seq = Observable(1, 2, 3, e) map {
      case n: Int => n
      case e: Exception => throw e
    }
    val obs = seq.recovered

    var actual = List[Try[Any]]()
    val sub = obs.subscribe { v =>
      actual = actual :+ v
    }

    val expected = List(Success(1), Success(2), Success(3), Failure(e))
    assert(actual === expected)
  }

  test("WikipediaApi should correctly use timedOut") {
    val requests = Observable.interval(1 second)
    val to = requests.timedOut(3)

    assert(to.toBlockingObservable.toList === List(0, 1))
  }

  test("WikipediaApi should correctly use timedOut when first completes") {
    val requests = Observable.interval(1 second).take(2)
    val to = requests.timedOut(4)

    assert(to.toBlockingObservable.toList === List(0, 1))
  }
  
  test("timedOut") {
    val seq = Observable(1, 2, 3).zip(Observable.interval(700 millis)).timedOut(1L)
    assert(seq.toBlockingObservable.toList === List((1, 0)))
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }
}