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

  test("demux test 0") {
    val in, out = new Wire
    demux0(in, List(out))
    in.setSignal(false)
    run
    assert(out.getSignal == false, "demux test 0: input=0 => output=0")
    in.setSignal(true)
    run
    assert(out.getSignal == true, "demux test 0: input=1 => output=1")
  }

  test("demux test 1>1") {
    val in, out = new Wire
    demux(in, Nil, List(out))
    in.setSignal(false)
    run
    assert(out.getSignal == false, "demux test 1>1: input=0 => output=0")
    in.setSignal(true)
    run
    assert(out.getSignal == true, "demux test 1>1: input=1 => output=1")
  }
  
  test("demux test 1>2") {
    val in, c, o0, o1 = new Wire
    demux(in, List(c), List(o1, o0))
    
    in.setSignal(false)
    c.setSignal(false)
    run
    
    assert(o0.getSignal == false, "demux test 1>2: input=0, control=0 => output0=0")
    assert(o1.getSignal == false, "demux test 1>2: input=0, control=0 => output1=0")
    
    in.setSignal(true)
    run
    
    assert(o0.getSignal == true, "demux test 1>2: input=1, control=0 => output0=1")
    assert(o1.getSignal == false, "demux test 1>2: input=1, control=0 => output1=0")
    
    c.setSignal(true)
    run
    
    assert(o0.getSignal == false, "demux test 1>2: input=1, control=1 => output0=0")
    assert(o1.getSignal == true, "demux test 1>2: input=1, control=1 => output1=1")
    
    in.setSignal(false)
    run
    
    assert(o0.getSignal == false, "demux test 1>2: input=0, control=1 => output0=0")
    assert(o1.getSignal == false, "demux test 1>2: input=0, control=1 => output1=0")
  }
  
  test("demux test 2>4") {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    demux(in, List(c1, c0), List(o3, o2, o1, o0))
    
    in.setSignal(false)
    c0.setSignal(false)
    c1.setSignal(false)
    run
    
    assert(o0.getSignal == false, "demux test 2>4: input=0, c0=0, c1=0 => o0=0")
    assert(o1.getSignal == false, "demux test 2>4: input=0, c0=0, c1=0 => o1=0")
    assert(o2.getSignal == false, "demux test 2>4: input=0, c0=0, c1=0 => o2=0")
    assert(o3.getSignal == false, "demux test 2>4: input=0, c0=0, c1=0 => o3=0")
    
    in.setSignal(true)
    run
    
    assert(o0.getSignal == true, "demux test 2>4: input=1, c0=0, c1=0 => o0=1")
    assert(o1.getSignal == false, "demux test 2>4: input=1, c0=0, c1=0 => o1=0")
    assert(o2.getSignal == false, "demux test 2>4: input=1, c0=0, c1=0 => o2=0")
    assert(o3.getSignal == false, "demux test 2>4: input=1, c0=0, c1=0 => o3=0")
    
    c1.setSignal(true)
    run
    
    assert(o0.getSignal == false, "demux test 2>4: input=1, c0=0, c1=1 => o0=0")
    assert(o1.getSignal == false, "demux test 2>4: input=1, c0=0, c1=1 => o1=0")
    assert(o2.getSignal == true, "demux test 2>4: input=1, c0=0, c1=1 => o2=1")
    assert(o3.getSignal == false, "demux test 2>4: input=1, c0=0, c1=1 => o3=0")
    
    in.setSignal(true)
    run
    
    assert(o0.getSignal == false, "demux test 2>4: input=0, c0=0, c1=1 => o0=0")
    assert(o1.getSignal == false, "demux test 2>4: input=0, c0=0, c1=1 => o1=0")
    assert(o2.getSignal == false, "demux test 2>4: input=0, c0=0, c1=1 => o2=0")
    assert(o3.getSignal == false, "demux test 2>4: input=0, c0=0, c1=1 => o3=0")
  }
}
