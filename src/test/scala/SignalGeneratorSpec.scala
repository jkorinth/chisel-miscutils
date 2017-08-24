package chisel.miscutils
import  generators._
import  org.scalacheck._, org.scalacheck.Prop._
import  org.scalatest.prop.Checkers
import  chisel3._
import  chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class SignalGeneratorWaveformTest(sg: SignalGenerator) extends PeekPokeTester(sg) {
  private var cc = 0

  // re-define step to output progress info
  override def step(n: Int) {
    if (sg.useInputAsClock) {
      super.step(1)
      poke(sg.io.in, (peek(sg.io.in) + 1) % 2)
      super.step(1)
      cc += 1
    } else {
      super.step(n)
      cc += n
    }
  }

  poke(sg.io.in, 0)
  reset(10)
  step(10)
  cc = 0

  for (i <- 0 until sg.signals.length) {
    println(s"signal ${i+1}/${sg.signals.length} started at $cc")
    for (s <- 0 until sg.signals(i).periods) {
      val e = sg.signals(i).value
      val p = peek(sg.io.v)
      expect(sg.io.v, sg.signals(i).value.B, s"expected $e at clock cycle $cc")
      step(1)
    }
    println(s"signal ${i+1}/${sg.signals.length} finished at $cc")
    step(1)
  }
}

class SignalGeneratorSpec extends ChiselFlatSpec with Checkers {
  behavior of "SignalGenerator"

  it should "generate arbitrary waveforms" in 
    check(forAll(signalGeneratorGen) { case (wave, useInputAsClock) =>
      println(s"useInputAsClock: $useInputAsClock, wave: $wave")
      Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/signalgenerator/spec"),
          () => new SignalGenerator(wave, useInputAsClock))
        { m => new SignalGeneratorWaveformTest(m) }
    })
}
