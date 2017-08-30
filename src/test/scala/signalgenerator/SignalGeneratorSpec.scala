package chisel.miscutils.signalgenerators
import  chisel.miscutils._
import  generators._
import  org.scalacheck._, org.scalacheck.Prop._
import  org.scalatest.prop.Checkers
import  chisel3._
import  chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import  SignalGenerator._
import  scala.util.Random

/** Connects a random clock signal generator to the main signal generator to test
 *  external clock port facilities.
 **/
class SignalGeneratorWithClockInput(val signals: Waveform) extends Module {
  val io  = IO(new Bundle { val v = Output(Bool()); val clk = Output(Bool()) })
  val clw = 0 to signals.length * 2 map (i => Signal(i % 2 == 0, 2 + Random.nextInt().abs % 10))
  val clk = Module(new SignalGenerator(clw))
  val sgn = Module(new SignalGenerator(signals, true))
  sgn.io.in := clk.io.v
  io.v      := sgn.io.v
  io.clk    := clk.io.v
}

/** Checks the correctness of the waveform output w.r.t. its signal input. */
class SignalGeneratorWithClockInputWaveformTest(sgc: SignalGeneratorWithClockInput) extends PeekPokeTester(sgc) {
  private var cc = 0
  reset(10)
  cc = 0
  step(1)

  /** Advance by one positive clock edge on the external clock. */
  def clockEdge {
    while (peek(sgc.io.clk) > 0)  step(1)
    while (peek(sgc.io.clk) == 0) step(1)
    step(1)  // one regular clock delay due to internal registers
    cc += 1
  }

  clockEdge

  for (i <- 0 until sgc.signals.length) {
    //println(s"signal ${i+1}/${sgc.signals.length} started at $cc")
    for (s <- 1 until sgc.signals(i).periods) {
      val e = sgc.signals(i).value
      val p = peek(sgc.io.v)
      expect(sgc.io.v, sgc.signals(i).value.B, s"expected $e at clock cycle $cc")
      clockEdge
    }
    //println(s"signal ${i+1}/${sgc.signals.length} finished at $cc")
    clockEdge
  }
}

class SignalGeneratorWaveformTest(sg: SignalGenerator) extends PeekPokeTester(sg) {
  require(!sg.useInputAsClock, "cannot set input as clock, use a SignalWithClockInputWaveformTest instead!")
  private var cc = 0

  // re-define step to output progress info
  override def step(n: Int) {
    super.step(n)
    cc += n
  }

  poke(sg.io.in, 0)
  reset(10)
  cc = 0
  step(1)
  var prev_input = peek(sg.io.in)

  for (i <- 0 until sg.signals.length) {
    //println(s"signal ${i+1}/${sg.signals.length} started at $cc")
    for (s <- 1 until sg.signals(i).periods) {
      val e = sg.signals(i).value
      val p = peek(sg.io.v)
      expect(sg.io.v, sg.signals(i).value.B, s"expected $e at clock cycle $cc")
      step(1)
    }
    //println(s"signal ${i+1}/${sg.signals.length} finished at $cc")
    step(1)
  }
}

class SignalGeneratorSpec extends ChiselFlatSpec with Checkers {
  behavior of "SignalGenerator"

  it should "generate arbitrary waveforms with regular clock" in 
    check(forAll(waveformGen()) { wave =>
      Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/signalgenerator/spec"),
          () => new SignalGenerator(wave, false))
        { m => new SignalGeneratorWaveformTest(m) }
    }, minSuccessful(100))

  it should "generate arbitrary waveforms with random clock" in
    check(forAll(waveformGen()) { wave =>
      Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/signalgenerator/spec"),
          () => new SignalGeneratorWithClockInput(wave))
        { m => new SignalGeneratorWithClockInputWaveformTest(m) }
    }, minSuccessful(25))
}
