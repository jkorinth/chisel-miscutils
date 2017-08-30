package chisel.miscutils
import  chisel3._
import  chisel3.util._

object SignalGenerator {
  sealed case class Signal(value: Boolean, periods: Int = 1) extends Ordered[Signal] {
    import scala.math.Ordered.orderingToOrdered
    def compare(that: Signal): Int = periods compare that.periods
    def unary_! = this.copy(value = !value)
  }
  final case class Waveform(signals: Seq[Signal]) {
    require (signals.length > 0, "waveform must not be empty")
  }
  implicit def waveformToSeq(w: Waveform): Seq[Signal] = w.signals
  implicit def seqToWaveform(s: Seq[Signal]): Waveform = Waveform.apply(s)

  implicit def makeSignal(sd: (Boolean, Int)): Signal = Signal(sd._1, sd._2)
  implicit def makeWaveform(ls: List[(Boolean, Int)]): Waveform = ls map makeSignal

  class IO extends Bundle {
    val v = Output(Bool())
    val in = Input(Bool())
  }
}

class SignalGenerator(val signals: SignalGenerator.Waveform,
                      val useInputAsClock: Boolean = false) extends Module {
  require (signals.length > 0, "Waveform must not be empty.")
  require (signals map (_.periods > 1) reduce (_&&_),
      "All signals must have at least two clock cycles length.")
  val io = IO(new SignalGenerator.IO)
  val cnts_rom = Vec(signals map (n => (n.periods - 1).U))
  val vals_rom = Vec(signals map (n => (n.value).B))
  val cnt      = RegInit(UInt(log2Ceil(signals.max.periods).W), init = cnts_rom(0))
  val curr_idx = RegInit(UInt(log2Ceil(signals.length).W), init = 0.U)
  val vreg     = RegInit(Bool(), init = vals_rom(0))

  io.v := vreg

  vreg := vals_rom(curr_idx)
  // trigger on either clock or pos input edge
  when (if (useInputAsClock) io.in && !RegNext(io.in) else true.B) {
    when (cnt === 0.U) {
      val next_idx = Mux(curr_idx < (signals.length - 1).U, curr_idx + 1.U, 0.U)
      curr_idx    := next_idx
      cnt         := cnts_rom(next_idx)
    }
    .otherwise {
      cnt := cnt - 1.U
    }
  }
}
