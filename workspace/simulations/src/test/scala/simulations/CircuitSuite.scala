package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")
    
    in1.setSignal(false)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 4")
  }
  
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")
    
    in1.setSignal(false)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 4")
  }

}
