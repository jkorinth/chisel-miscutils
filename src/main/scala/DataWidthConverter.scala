package chisel.miscutils
import  chisel3._
import  chisel3.util._

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
                         val littleEndian: Boolean = true) extends Module {
  require (inWidth > 0, "inWidth must be > 0")
  require (outWidth > 0, "inWidth must be > 0")
  require (inWidth != outWidth, "inWidth (%d) must be different from outWidth (%d)"
             .format(inWidth, outWidth))
  require (inWidth % outWidth == 0 || outWidth % inWidth == 0,
           "inWidth (%d) and outWidth (%d) must be integer multiples of each other"
             .format(inWidth, outWidth))

  val io = IO(new Bundle {
    val inq = Flipped(Decoupled(UInt(inWidth.W)))
    val deq = Decoupled(UInt(outWidth.W))
  })

  val ratio: Int = if (inWidth > outWidth) inWidth / outWidth else outWidth / inWidth
  val d_w = if (inWidth > outWidth) inWidth else outWidth // data register width

  if (inWidth > outWidth)
    downsize()
  else
    upsize()

  private def upsize() = {
    val i = Reg(UInt(log2Ceil(ratio + 1).W))
    val d = Reg(UInt(outWidth.W))

    io.inq.ready := !reset && (i =/= 0.U || (io.inq.valid && io.deq.ready))
    io.deq.bits  := d
    io.deq.valid := !reset && i === 0.U

    when (reset) {
      i := ratio.U
      d := 0.U
    }
    .otherwise {
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
  }

  private def downsize() = {
    val i = Reg(UInt(log2Ceil(ratio + 1).W))
    val d = Reg(UInt(inWidth.W))

    io.inq.ready := !reset && (i === 0.U || (i === 1.U && io.deq.ready))
    if (littleEndian)
      io.deq.bits := d(outWidth - 1, 0)
    else
      io.deq.bits := d(inWidth - 1, inWidth - outWidth)
    io.deq.valid := !reset && i > 0.U

    when (reset) {
      i := 0.U
      d := 0.U
    }
    .otherwise {
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
}

