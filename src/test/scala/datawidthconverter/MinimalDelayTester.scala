package chisel.miscutils.datawidthconverter
import  chisel.miscutils._
import  chisel3._, chisel3.util._
import  chisel3.iotesters.PeekPokeTester
import  math.pow

/** Generic tester for [[MinimalDelayHarness]]:
  * Performs the same check as [[CorrectnessTester]], but without the slow
  * queue in between; in this scenario, the DataWidthConverter under test may
  * either never block on the in-queue (upsizing) or always be valid at the
  * out-queue (downsizing). This guarantess maximal throughput for the selected
  * conversion ratio.
  **/
class MinimalDelayTester[T <: UInt](m: MinimalDelayHarness) extends PeekPokeTester(m) {
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
        val e = peek(m.io.dsrc_out_bits)
        expecteds = expecteds :+ e
        println ("adding expected value: %d (%s)".format(e, toBinaryString(e, m.dwc.inWidth)))
      }

      // check output element: must match head of expecteds
      if (peek(m.io.dwc2_deq_valid) > 0 && peek(m.io.dwc2_deq_ready) > 0) {
        firstOutputReceived = true
        val v = peek(m.io.dwc2_deq_bits)
        if (expecteds.isEmpty) {
          val errmsg = "received value output value %d (%s), but none expected yet"
            .format(v, toBinaryString(v, m.dwc.inWidth))
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
