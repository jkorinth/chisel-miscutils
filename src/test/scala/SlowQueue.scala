package chisel.miscutils
import  chisel3._
import  chisel3.util._

/** A slow queue which delays each element by a configurable delay. */
class SlowQueue(width: Int, val delay: Int = 10) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(UInt(width.W)))
    val deq = Decoupled(UInt(width.W))
    val dly = Input(UInt(log2Ceil(delay).W))
  })

  val waiting :: ready :: Nil = Enum(2)
  val state = RegInit(ready)

  val wr = Reg(UInt(log2Ceil(delay).W))

  io.deq.bits  := io.enq.bits
  io.enq.ready := io.deq.ready && state === ready
  io.deq.valid := io.enq.valid && state === ready

  when (reset) {
    state := ready
  }
  .otherwise {
    when (state === ready && io.enq.ready && io.deq.valid) {
      state := waiting
      wr    := io.dly
    }
    when (state === waiting) {
      wr := wr - 1.U
      when (wr === 0.U) { state := ready }
    }
  }
}

