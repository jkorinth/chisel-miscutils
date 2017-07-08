package chisel.miscutils
import  chisel3._
import  chisel3.util._
import  chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import  SignalGenerator._

class SignalGeneratorComposition1 extends Module {
  val waveform: SignalGenerator.Waveform =
      (for (i <- 2 until 30) yield List((false, 5), (true, i))) reduce (_++_)
  val clock_sg = Module(new SignalGenerator(List((true, 2), (false, 2))))
  val test_sg  = Module(new SignalGenerator(waveform, true))
  val io       = IO(new Bundle { val v = Output(Bool()) })
  test_sg.io.in := clock_sg.io.v
  io.v          := test_sg.io.v
}

class SignalGeneratorSuite extends ChiselFlatSpec {
  "test1" should "be ok" in {
    val waveform: SignalGenerator.Waveform =
        (for (i <- 2 until 30) yield List((false, 5), (true, i))) reduce (_++_)
    Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/signalgenerator"),
                   () => new SignalGenerator(waveform))
      { m => new SignalGeneratorTest(m) }
  }

  "test2" should "be ok" in { // same as test1, but with user supplied clock
    Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/signalgenerator"),
                   () => new SignalGeneratorComposition1)
      { m => new SignalGeneratorComposition1Test(m) }
  }
}

class SignalGeneratorTest(sg: SignalGenerator) extends PeekPokeTester(sg) {
  import scala.util.Properties.{lineSeparator => NL}
  private var cc = 0

  // re-define step to output progress info
  override def step(n: Int) {
    super.step(n)
    cc += n
    if (cc % 1000 == 0) println("clock cycle: " + cc)
  }

  // waits for next positive edge on signal
  def waitForPosEdge[T <: Chisel.Bits](s: T) {
    while(peek(s) > 0) step(1)
    while(peek(s) == 0) step(1)
  }

  // waits for next positive edge on signal
  def waitForNegEdge[T <: Chisel.Bits](s: T) {
    while(peek(s) == 0) step(1)
    while(peek(s) > 0) step(1)
  }

  reset(10)

  for (j <- 0 to 1) {
    for (i <- 2 until 30) {
      waitForPosEdge(sg.io.v)
      val cc_start = cc
      waitForNegEdge(sg.io.v)
      expect (cc - cc_start == i, "wrong number of clock cycles")
    }
  }
}

class SignalGeneratorComposition1Test(sg: SignalGeneratorComposition1) extends PeekPokeTester(sg) {
  private var cc = 0

  // re-define step to output progress info
  override def step(n: Int) {
    super.step(n)
    cc += n
    if (cc % 1000 == 0) println("clock cycle: " + cc)
  }

  // waits for next positive edge on signal
  def waitForPosEdge[T <: Chisel.Bits](s: T) {
    while(peek(s) > 0) step(1)
    while(peek(s) == 0) step(1)
  }

  // waits for next positive edge on signal
  def waitForNegEdge[T <: Chisel.Bits](s: T) {
    while(peek(s) == 0) step(1)
    while(peek(s) > 0) step(1)
  }

  reset(10)

  for (j <- 0 to 1) {
    for (i <- 2 until 30) {
      waitForPosEdge(sg.io.v)
      val cc_start = cc
      waitForNegEdge(sg.io.v)
      expect (cc - cc_start == i * 4, "wrong number of clock cycles")
    }
  }
}
