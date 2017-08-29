package chisel.miscutils
import  chisel3._
import  chisel3.util._

object DecoupledDataSource {
  /**
   * Interface for DecoupledDataSource.
   **/
  class IO[T <: Data](gen: T) extends Bundle {
    val out = Decoupled(gen.cloneType)
  }
}

/**
 * Data source providing fixed data via Decoupled interface.
 * Provides the data given via Decoupled handshakes; if repeat
 * is true, data is wrapped around.
 * @param gen Type.
 * @param size Total number of elements.
 * @param data Function providing data for each index
 * @param repeat If true, will always have data via wrap-around,
                 otherwise valid will go low after data was
		 consumed.
 **/
class DecoupledDataSource[T <: Data](gen: T,
                                     val size : Int,
                                     val data: (Int) => T,
                                     val repeat: Boolean = true)
                                    (implicit l: Logging.Level)
    extends Module with Logging {
  cinfo("size = %d, repeat = %s, addrWidth = %d".format(size,
    if (repeat) "true" else "false", log2Ceil(if (repeat) size else size + 1)))

  val ds  = for (i <- 0 until size) yield data(i) // evaluate data to array
  val io  = IO(new DecoupledDataSource.IO(gen)) // interface
  val i   = Reg(UInt(log2Ceil(if (repeat) size else size + 1).W)) // index
  val rom = Vec.tabulate(size)(n => ds(n)) // ROM with data
  io.out.bits  := rom(i) // current index data
  io.out.valid := !reset && (i < size.U) // valid until exceeded
  when (reset) {
    i := 0.U
  }
  .otherwise {
    when (io.out.ready && io.out.valid) {
      val next = if (repeat) {
        if (math.pow(2, log2Ceil(size)).toInt == size) {
          i + 1.U
        } else {
          Mux((i + 1.U) < size.U, i + 1.U, 0.U)
        }
      } else {
        Mux(i < size.U, i + 1.U, i)
      }
      info(p"i = $i -> $next, bits = 0x${Hexadecimal(io.out.bits.asUInt())}")
      i := next
    }
  }
}
