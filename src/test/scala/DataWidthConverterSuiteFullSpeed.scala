package chisel.miscutils
import  chisel3._
import  chisel3.util._
import  chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import  scala.math._
import  java.nio.file.Paths

/**
 * DataWidthConverterHarness: Correctness test harness.
 * A DecoupledDataSource with random data is connected to a pair
 * of data width converters with inverted params. This circuit
 * must behave exactly like a delay on the input stream (where
 * the length of the delay is 2 * in/out-width-ratio).
 **/
class DataWidthConverterHarnessFullSpeed(val inWidth: Int, val outWidth: Int, val littleEndian: Boolean) extends Module {
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

/**
 * Generic tester for DataWidthConverterHarness:
 * Uses DataWidthConverterHarness class to check output correctness.
 * Tracks incoming data from the data source in expecteds list.
 * Whenever output is valid, it is compared to the expecteds,
 * mismatches are reported accordingly.
 * Does NOT check timing, only correctness of the output values.
 **/
class DataWidthConverterFullSpeed[T <: UInt](m: DataWidthConverterHarnessFullSpeed) extends PeekPokeTester(m) {
  import scala.util.Properties.{lineSeparator => NL}

  // returns binary string for Int, e.g., 0011 for 3, width 4
  private def toBinaryString(n: BigInt, width: Int) =
    "%%%ds".format(width).format(n.toString(2)).replace(' ', '0')

  /** Performs data correctness at full speed check. **/
  def check() = {
    var i = 0
    var firstOutputReceived = false
    var expecteds: List[BigInt] = List()
    def running = peek(m.io.dsrc_out_valid) > 0 ||
                  peek(m.io.dwc_inq_valid) > 0 ||
                  peek(m.io.dwc_deq_valid) > 0 ||
                  peek(m.io.dwc2_inq_valid) > 0 ||
                  peek(m.io.dwc2_deq_valid) > 0
    while (running) {
      // scan output element and add to end of expected list
      if (peek(m.io.dsrc_out_valid) > 0 && peek(m.io.dwc_inq_ready) > 0) {
        //val e = peek(m.dsrc.io.out.bits)
        val e = peek(m.io.dsrc_out_bits)
        expecteds = expecteds :+ e
        println ("adding expected value: %d (%s)".format(e, toBinaryString(e, m.dwc.inWidth)))
      }

      // check output element: must match head of expecteds
      if (peek(m.io.dwc2_deq_valid) > 0 && peek(m.io.dwc2_deq_ready) > 0) {
        firstOutputReceived = true
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

      // check: if upsizing, inq may never block
      if (m.inWidth < m.outWidth) {
        val error = peek(m.io.dwc_inq_ready) == 0 && peek(m.io.dwc_inq_valid) != 0
        if (error)
          println("ERROR: input queue may never block while input is available")
        expect(!error, "upsizing: input queue may not block while input is available")
      }

      // check: if downsizing, deq must remain valid until end
      if (firstOutputReceived && !expecteds.isEmpty && m.inWidth > m.outWidth) {
        if (peek(m.io.dwc_deq_valid) == 0)
          println("ERROR: output queue must remain valid after first element")
        if (peek(m.io.dwc_deq_ready) == 0)
          println("ERROR: output queue must remain ready after first element")
        expect(peek(m.io.dwc_deq_ready) != 0, "downsizing: output queue must remain ready after first")
        expect(peek(m.io.dwc_deq_valid) != 0, "downsizing: output queue must remain valid after first")
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
class DataWidthConverterSuiteFullSpeed extends ChiselFlatSpec {
  def resize(inWidth: Int, outWidth: Int, littleEndian: Boolean = true) = {
    println("testing conversion of %d bit to %d bit, %s ..."
        .format(inWidth, outWidth, if (littleEndian) "little-endian" else "big-endian"))
    val dir = Paths.get("test")
                   .resolve("dwc_fullspeed")
                   .resolve("%dto%d%s".format(inWidth, outWidth, if (littleEndian) "le" else "be"))
                   .toString
    Driver.execute(Array("--fint-write-vcd", "--target-dir", dir, "--no-dce"),
                   () => new DataWidthConverterHarnessFullSpeed(inWidth, outWidth, littleEndian))
      { m => new DataWidthConverterFullSpeed(m) }
  }

  // simple test group, can be used for waveform analysis
  /*"check16to4le" should "be ok" in  { resize(16,  4, true) }
  "check4to16le" should "be ok" in  { resize(4,  16, true) }
  "check16to4be" should "be ok" in  { resize(16,  4, false) }
  "check4to16be" should "be ok" in  { resize(4,  16, false) }
  "check64to32be" should "be ok" in  { resize(64,  32, false) }
  "check32to64be" should "be ok" in  { resize(32,  64, false) }*/

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
