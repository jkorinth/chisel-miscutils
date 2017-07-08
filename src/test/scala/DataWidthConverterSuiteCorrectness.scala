package chisel.miscutils
import  chisel3._
import  chisel3.util._
import  chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import  org.scalatest.junit.JUnitSuite
import  scala.math._
import  java.nio.file.Paths

class SlowQueue(width: Int, val delay: Int = 10) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(UInt(width.W)))
    val deq = Decoupled(UInt(width.W))
    val dly = Input(UInt(log2Ceil(delay).W))
  })

  val waiting :: ready :: Nil = Enum(2)
  val state = RegInit(init = ready)

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

/**
 * DataWidthConverterHarness: Correctness test harness.
 * A DecoupledDataSource with random data is connected to a pair
 * of data width converters with inverted params. This circuit
 * must behave exactly like a delay on the input stream (where
 * the length of the delay is 2 * in/out-width-ratio).
 * There's a slow queue in-between to simulate receivers with
 * varying speed of consumption.
 **/
class DataWidthConverterHarness(inWidth: Int, outWidth: Int, littleEndian: Boolean, delay: Int = 10) extends Module {
  val io = IO(new Bundle {
    val dly = Input(UInt(log2Ceil(delay).W))
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

/**
 * Generic tester for DataWidthConverterHarness:
 * Uses DataWidthConverterHarness class to check output correctness.
 * Tracks incoming data from the data source in expecteds list.
 * Whenever output is valid, it is compared to the expecteds,
 * mismatches are reported accordingly.
 * Does NOT check timing, only correctness of the output values.
 **/
class DataWidthConverterCorrectnessTester[T <: UInt](m: DataWidthConverterHarness) extends PeekPokeTester(m) {
  import scala.util.Properties.{lineSeparator => NL}

  // returns binary string for Int, e.g., 0011 for 3, width 4
  private def toBinaryString(n: BigInt, width: Int) =
    "%%%ds".format(width).format(n.toString(2)).replace(' ', '0')

  /** Performs data correctness check. **/
  def check() = {
    var i = 0
    var delay = m.slq.delay - 1
    poke (m.io.dly, delay)
    var expecteds: List[BigInt] = List()
    def running = peek(m.io.dsrc_out_valid) > 0 ||
                  peek(m.io.dwc_inq_valid) > 0 ||
                  peek(m.io.dwc_deq_valid) > 0 ||
                  peek(m.io.dwc2_inq_valid) > 0 ||
                  peek(m.io.dwc2_deq_valid) > 0
    while (running) {
      // scan output element and add to end of expected list
      if (peek(m.io.dsrc_out_valid) > 0 && peek(m.io.dwc_inq_ready) > 0) {
        val e = peek(m.io.dsrc_out_bits)
        expecteds = expecteds :+ e
        println ("adding expected value: %d (%s)".format(e, toBinaryString(e, m.dwc.inWidth)))
      }

      // check output element: must match head of expecteds
      if (peek(m.io.dwc2_deq_valid) > 0 && peek(m.io.dwc2_deq_ready) > 0) {
        // update delay (decreasing with each output)
        delay = if (delay == 0) m.slq.delay - 1 else delay - 1
        poke(m.io.dly, delay)
        // check output
        val v = peek(m.io.dwc2_deq_bits)
        if (expecteds.isEmpty) {
          val errmsg = "received value output value %d (%s), but none expected yet".format(
            v, toBinaryString(v, m.dwc.inWidth))
          println (errmsg)
          expect(false, errmsg)
        } else {
          if (v == expecteds.head) {
            println ("element #%d ok!".format(i))
          } else  {
            val errmsg = "element #%d wrong: expected %d (%s), found %d (%s)".format(
                i, expecteds.head, toBinaryString(expecteds.head, m.dwc.inWidth),
                v, toBinaryString(v, m.dwc.inWidth))
            println (errmsg)
            expect(v == expecteds.head, errmsg)
          }
          expecteds = expecteds.tail
        }
        i += 1
      }

      // advance sim
      step (1)
    }
  }

  reset(10) // reset for 10 cycles
  check()
  step (20) // settle output
}

/** Unit test for DataWidthConverter hardware. **/
class DataWidthConverterSuite extends ChiselFlatSpec {
  def resize(inWidth: Int, outWidth: Int, littleEndian: Boolean = true) = {
    println("testing conversion of %d bit to %d bit, %s ..."
        .format(inWidth, outWidth, if (littleEndian) "little-endian" else "big-endian"))
    val dir = Paths.get("test")
                   .resolve("DataWidthConverterSuite")
                   .resolve("%dto%d%s".format(inWidth, outWidth, if (littleEndian) "le" else "be"))
                   .toString
    Driver.execute(Array("--fint-write-vcd", "--target-dir", dir),
                   () => new DataWidthConverterHarness(inWidth, outWidth, littleEndian))
      { m => new DataWidthConverterCorrectnessTester(m) }
  }

  // simple test group, can be used for waveform analysis
  /*"check16to4le" should "be ok" in  { resize(16,  4, true) }
  "check4to16le" should "be ok" in  { resize(4,  16, true) }
  "check16to4be" should "be ok" in  { resize(16,  4, false) }
  "check4to16be" should "be ok" in  { resize(4,  16, false) }
  "check64to32be" should "be ok" in  { resize(64,  32, false) }*/

  // downsizing tests
  "check2to1le" should "be ok" in   { resize(2,   1, true) }
  "check2to1be" should "be ok" in   { resize(2,   1, false) }
  "check8to1le" should "be ok" in   { resize(8,   1, true) }
  "check8to1be" should "be ok" in   { resize(8,   1, false) }
  "check16to4le" should "be ok" in  { resize(16,  4, true) }
  "check16to4be" should "be ok" in  { resize(16,  4, false) }
  "check16to8le" should "be ok" in  { resize(16,  8, true) }
  "check16to8be" should "be ok" in  { resize(16,  8, false) }
  "check32to8le" should "be ok" in  { resize(32,  8, true) }
  "check32to8be" should "be ok" in  { resize(32,  8, false) }
  "check64ot8le" should "be ok" in  { resize(64,  8, true) }
  "check64to8be" should "be ok" in  { resize(64,  8, false) }
  "check64ot32le" should "be ok" in { resize(64, 32, true) }
  "check64to32be" should "be ok" in { resize(64, 32, false) }

  // upsizing tests
  "check1to2le" should "be ok" in   { resize(1,   2, true) }
  "check1to2be" should "be ok" in   { resize(1,   2, false) }
  "check1to8le" should "be ok" in   { resize(1,   8, true) }
  "check1to8be" should "be ok" in   { resize(1,   8, false) }
  "check4to16le" should "be ok" in  { resize(4,  16, true) }
  "check4to16be" should "be ok" in  { resize(4,  16, false) }
  "check8to16le" should "be ok" in  { resize(8,  16, true) }
  "check8to16be" should "be ok" in  { resize(8,  16, false) }
  "check8to32le" should "be ok" in  { resize(8,  32, true) }
  "check8to32be" should "be ok" in  { resize(8,  32, false) }
  "check8ot64le" should "be ok" in  { resize(8,  64, true) }
  "check8to64be" should "be ok" in  { resize(8,  64, false) }
  "check32ot64le" should "be ok" in { resize(32, 64, true) }
  "check32to64be" should "be ok" in { resize(32, 64, false) }
}
