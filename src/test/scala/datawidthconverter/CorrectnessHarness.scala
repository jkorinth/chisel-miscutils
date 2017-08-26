package chisel.miscutils.datawidthconverter
import  chisel.miscutils._
import  chisel3._
import  chisel3.util._
import  math.pow

/** Correctness test harness for DataWidthConverter:
  * A DecoupledDataSource with random data is connected to a pair
  * of data width converters with inverted params. This circuit
  * must behave exactly like a delay on the input stream (where
  * the length of the delay is 2 * in/out-width-ratio).
  * There's a slow queue in-between to simulate receivers with
  * varying speed of consumption.
  * @param inWidth Bit width of input data.
  * @param outWidth Bit width of output data (in and out must be
  *                 integer multiples/fractions of each other)
  * @param littleEndian Byte-endianess.
  * @param delay Clock cycle delay in [[SlowQueue]].
  */
class CorrectnessHarness(inWidth: Int,
                         outWidth: Int,
                         littleEndian: Boolean,
                         delay: Int = 10)
                        (implicit logLevel: Logging.Level) extends Module {
  require (delay > 0, "delay bitwidth must be > 0")
  val io = IO(new Bundle {
    val dly = Input(UInt(Seq(log2Ceil(delay), 1).max.W))
    val dsrc_out_valid = Output(Bool())
    val dsrc_out_bits = Output(UInt())
    val dwc_inq_valid = Output(Bool())
    val dwc_inq_ready = Output(Bool())
    val dwc_deq_valid = Output(Bool())
    val dwc_deq_ready = Output(Bool())
    val dwc2_inq_valid = Output(Bool())
    val dwc2_deq_valid = Output(Bool())
    val dwc2_deq_ready = Output(Bool())
    val dwc2_deq_bits = Output(UInt())
  })
  val dwc  = Module(new DataWidthConverter(inWidth, outWidth, littleEndian))
  val dsrc = Module(new DecoupledDataSource(UInt(inWidth.W),
                                            Seq(Seq(pow(2, inWidth).toLong, dwc.ratio).max, 500.toLong).min.toInt,
                                            n => (scala.math.random * pow(2, inWidth)).toLong.U,
                                            repeat = false))
  val dwc2 = Module(new DataWidthConverter(outWidth, inWidth, littleEndian))
  val slq  = Module(new SlowQueue(outWidth, delay))

  dwc.io.inq       <> dsrc.io.out
  slq.io.enq       <> dwc.io.deq
  slq.io.dly       := io.dly
  dwc2.io.inq      <> slq.io.deq
  dwc2.io.deq.ready := true.B

  // internal peek-and-poke does not work, need to wire as outputs:
  io.dsrc_out_valid := dsrc.io.out.valid
  io.dsrc_out_bits  := dsrc.io.out.bits
  io.dwc_inq_valid  := dwc.io.inq.valid
  io.dwc_inq_ready  := dwc.io.inq.ready
  io.dwc_deq_valid  := dwc.io.deq.valid
  io.dwc_deq_ready  := dwc.io.deq.ready
  io.dwc2_inq_valid := dwc2.io.inq.valid
  io.dwc2_deq_valid := dwc2.io.deq.valid
  io.dwc2_deq_ready := dwc2.io.deq.ready
  io.dwc2_deq_bits  := dwc2.io.deq.bits
}
