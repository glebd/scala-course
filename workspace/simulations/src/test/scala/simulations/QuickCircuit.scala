package simulations

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object QuickCircuit extends Properties("Circuit") {

  import Circuit.{andGate, orGate, orGate2, demux,run}

  property("and gate") = forAll {
    (sIn1: Boolean, sIn2: Boolean) =>
      val in1, in2, out = new Wire
      andGate(in1, in2, out)
      in1.setSignal(sIn1)
      in2.setSignal(sIn2)
      run
      out.getSignal == (sIn1 & sIn2)
  }

  property("or gate") = forAll {
    (sIn1: Boolean, sIn2: Boolean) =>
      val in1, in2, out = new Wire
      orGate(in1, in2, out)
      in1.setSignal(sIn1)
      in2.setSignal(sIn2)
      run
      out.getSignal == (sIn1 | sIn2)
  }

  property("or2 gate") = forAll {
    (sIn1: Boolean, sIn2: Boolean) =>
      val in1, in2, out = new Wire
      orGate2(in1, in2, out)
      in1.setSignal(sIn1)
      in2.setSignal(sIn2)
      run
      out.getSignal == (sIn1 | sIn2)
    }

  val genBoolean = Gen.oneOf(Gen.value(true), Gen.value(false))
  val genShortLitBoolean = for {
    n <- Gen.choose(0, 8) // More than 8 gets slooooooooooow ...
    g <- Gen.listOfN(n, genBoolean)
  } yield g

  def signalToBit(s: Boolean) =
    if (s) 1 else 0

  def controlToBitNumber(bs: List[Boolean]) =
    bs.foldLeft(0){
      case (acc, b) => 2 * acc + signalToBit(b) }

  def setWiresToSignals(ws: List[Wire], ss: List[Boolean]) =
    (ws zip ss) foreach { case (w, s) => w setSignal s }

  def countOneBits(ws: List[Wire]) =
    ws.map(_.getSignal).filter(identity).length

  property("demux") = forAll (genBoolean, genShortLitBoolean) {
    (sIn: Boolean, sControl: List[Boolean]) =>
      val numBitsControl = sControl.length
      val numBitsOutput = 1 << numBitsControl
      val wIn = new Wire
      val wControl = List.fill(numBitsControl){new Wire}
      val wOutput = List.fill(numBitsOutput){new Wire}
    
      demux(wIn, wControl, wOutput)
      wIn.setSignal(sIn)
      setWiresToSignals(wControl, sControl)
      run

      val numBitInOut = numBitsOutput - 1 - controlToBitNumber(sControl)
      val targetBitShouldBeInput = wOutput(numBitInOut).getSignal == sIn

      targetBitShouldBeInput && countOneBits(wOutput) == signalToBit(sIn)
  }
}
