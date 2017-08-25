package chisel.miscutils.datawidthconverter
import  chisel.miscutils._
import  chisel3._, chisel3.util._
import  math.pow

/** Minimal delay test harness for DataWidthConverter:
  * A DecoupledDataSource with random data is connected to a pair
  * of data width converters with inverted params. This circuit
  * must behave exactly like a delay on the input stream (where
  * the length of the delay is 2 * in/out-width-ratio).
  * @param inWidth Bit width of input data.
  * @param outWIdth Bit width of output data (must be integer
  *                 multiple/fraction of input width)
  * @param littleEndianess Byte-endianess.
  **/
class MinimalDelayHarness(val inWidth: Int,
                          val outWidth: Int,
			  val littleEndian: Boolean) extends Module {
  val io = IO(new Bundle {
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
                                            //n => UInt(n % pow(2, inWidth).toInt + 1, width = inWidth),
                                            n => (scala.math.random * pow(2, inWidth)).toLong.U,
                                            repeat = false))
  val dwc2 = Module(new DataWidthConverter(outWidth, inWidth, littleEndian))
  dwc.io.inq       <> dsrc.io.out
  dwc2.io.inq      <> dwc.io.deq
  dwc2.io.deq.ready := !reset

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
