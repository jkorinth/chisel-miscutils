package chisel.miscutils
import  chisel3._
import  chisel3.util._

object DataWidthConverter {
  class IO(inWidth: Int, outWidth: Int) extends Bundle {
    val inq = Flipped(Decoupled(UInt(inWidth.W)))
    val deq = Decoupled(UInt(outWidth.W))
  }
}

/**
 * DataWidthConverter converts the data width of a Queue.
 * Output is provided via a Queue, with increased or decreased
 * data rate, depending on the direction of the conversion.
 * Note: This would be much more useful, if the two Queues
 *       could use different clocks, but multi-clock support
 *       in Chisel is currently unstable.
 * @param inWidth Data width of input DecoupledIO (bits).
 * @param outWidth Data width of output DecoupledIO (bits); must
 *                 be integer multiples of each other.
 * @param littleEndian if inWidth &gt; outWidth, determines
 *                     the order of the nibbles (low to high)
 **/
class DataWidthConverter(val inWidth: Int,
                         val outWidth: Int,
                         val littleEndian: Boolean = true)
                        (implicit logLevel: Logging.Level) extends Module with Logging {
  require (inWidth > 0, "inWidth must be > 0")
  require (outWidth > 0, "inWidth must be > 0")
  require (inWidth != outWidth, "inWidth (%d) must be different from outWidth (%d)"
             .format(inWidth, outWidth))
  require (inWidth % outWidth == 0 || outWidth % inWidth == 0,
           "inWidth (%d) and outWidth (%d) must be integer multiples of each other"
             .format(inWidth, outWidth))

  cinfo(s"inWidth = $inWidth, outWidth = $outWidth, littleEndian = $littleEndian")

  val io = IO(new DataWidthConverter.IO(inWidth, outWidth))

  val ratio: Int = if (inWidth > outWidth) inWidth / outWidth else outWidth / inWidth
  val d_w = if (inWidth > outWidth) inWidth else outWidth // data register width

  if (inWidth > outWidth)
    downsize()
  else
    upsize()

  private def upsize() = {
    val i = RegInit(UInt(log2Ceil(ratio + 1).W), init = ratio.U)
    val d = RegInit(UInt(outWidth.W), 0.U)

    io.inq.ready := i =/= 0.U || (io.inq.valid && io.deq.ready)
    io.deq.bits  := d
    io.deq.valid := i === 0.U

    when (io.inq.ready && io.inq.valid) {
      if (littleEndian)
        d := Cat(io.inq.bits, d) >> inWidth.U
      else
        d := (d << inWidth.U) | io.inq.bits
      i := i - 1.U
    }
    when (io.deq.valid && io.deq.ready) {
      i := Mux(io.inq.valid, (ratio - 1).U, ratio.U)
    }
  }

  private def downsize() = {
    val i = RegInit(UInt(log2Ceil(ratio + 1).W), init = 0.U)
    val d = RegInit(UInt(inWidth.W), init = 0.U)

    io.inq.ready := i === 0.U || (i === 1.U && io.deq.ready)
    io.deq.valid := i > 0.U
    if (littleEndian)
      io.deq.bits := d(outWidth - 1, 0)
    else
      io.deq.bits := d(inWidth - 1, inWidth - outWidth)

    when (i > 0.U && io.deq.ready) {
      if (littleEndian)
        d := d >> outWidth.U
      else
        d := d << outWidth.U
      i := i - 1.U
    }
    when (io.inq.ready && io.inq.valid) {
      d := io.inq.bits
      i := ratio.U
    }
  }
}

