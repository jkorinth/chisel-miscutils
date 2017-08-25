package chisel.miscutils.datawidthconverter
import  chisel.miscutils._
import  chisel3._, chisel3.util._
import  chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

/** Generic tester for [[CorrectnessHarness]]:
  * Uses DataWidthConverterHarness class to check output correctness.
  * Tracks incoming data from the data source in expecteds list.
  * Whenever output is valid, it is compared to the expecteds,
  * mismatches are reported accordingly.
  * Does NOT check timing, only correctness of the output values.
  **/
class CorrectnessTester[T <: UInt](m: CorrectnessHarness) extends PeekPokeTester(m) {
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
                  peek(m.io.dwc_inq_valid) > 0  ||
                  peek(m.io.dwc_deq_valid) > 0  ||
                  peek(m.io.dwc2_inq_valid) > 0 ||
                  peek(m.io.dwc2_deq_valid) > 0
    while (running) {
      // scan output element and add to end of expected list
      if (peek(m.io.dsrc_out_valid) > 0 && peek(m.io.dwc_inq_ready) > 0) {
        val e = peek(m.io.dsrc_out_bits)
        expecteds = expecteds :+ e
        //println ("adding expected value: %d (%s)".format(e, toBinaryString(e, m.dwc.inWidth)))
      }

      // check output element: must match head of expecteds
      if (peek(m.io.dwc2_deq_valid) > 0 && peek(m.io.dwc2_deq_ready) > 0) {
        // update delay (decreasing with each output)
        delay = if (delay == 0) m.slq.delay - 1 else delay - 1
        poke(m.io.dly, delay)
        // check output
        val v = peek(m.io.dwc2_deq_bits)
        if (expecteds.isEmpty) {
          val errmsg = "received value output value %d (%s), but none expected yet"
            .format(v, toBinaryString(v, m.dwc.inWidth))
          println (errmsg)
          expect(false, errmsg)
        } else {
          if (v == expecteds.head) {
            // println ("element #%d ok!".format(i))
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
