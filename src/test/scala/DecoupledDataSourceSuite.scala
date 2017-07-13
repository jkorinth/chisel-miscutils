package chisel.miscutils
import  chisel3._
import  chisel3.util._
import  chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

/**
 * Tester class for DecoupledDataSource.
 * Automatically exhausts the internal data of the instance, checks that
 * each dequeued output matches the expected value. Also checks whether
 * or not the module wraps correctly (in case repeat is true).
 **/
class DecoupledDataSource_OutputCheck[T <: UInt](m: DecoupledDataSource[T], data: Int => Int) extends PeekPokeTester(m) {
  import scala.util.Properties.{lineSeparator => NL}

  poke(m.io.out.ready, true)
  var errors: List[String] = List()
  var i = 0
  while (peek(m.io.out.valid) > 0 && i <= m.size) {
    if (i >= m.size) {
      if (! m.repeat)
        errors = "repeat is false, but index (%d) exceeds size(%d)".format(i, m.size) :: errors
    } else {
      if (peek(m.io.out.bits) != m.data(i)) {
        errors = "output #%d: expected %d, found %d".format(i, data(i), peek(m.io.out.bits)) :: errors
      } else {
        // wait for random time up to 10 cycles
	val wait = (scala.math.random * 10).toInt
	poke(m.io.out.ready, false)
	step(wait)
	poke(m.io.out.ready, true)
      }
    }
    i += 1
    step(1)
  }
  expect (errors.length == 0, ("all elements should match, errors: " :: errors).mkString(NL))
}

class DecoupledDataSourceSuite extends ChiselFlatSpec {
  /** Performs randomized tests with random data of random size. **/
  "checkRandomOutputs" should "be ok" in {
    for (i <- 0 until 1) {
      val cnt    = Seq(1, (scala.math.random * 1000).toInt).max
      val width  = Seq(1, (scala.math.random * 64).toInt).max
      val repeat = scala.math.random > 0.5
      val data   = for (i <- 0 until cnt) yield (scala.math.random  * scala.math.pow(2, width)).toInt
      println("testing cnt = %d, width = %d, repeat = %b ...".format(cnt, width, repeat))
      Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/DecoupledDataSourceSuite/checkRandomOutputs"),
                     () => new DecoupledDataSource(UInt(width.W), cnt, i => data(i).U, repeat))
        { m => new DecoupledDataSource_OutputCheck(m, data) }
    }
  }

  /** Performs test for 8bit wide sequential data with repeat. **/
  "checkSequentialOutputsWithRepeat" should "be ok" in {
    val data = 0 until 256
    Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/DecoupledDataSourceSuite/checkSequentialOutputsWithRepeat"),
                   () => new DecoupledDataSource(UInt(8.W), 256, i => data(i).U, true))
      { m => new DecoupledDataSource_OutputCheck(m, data) }
  }

  /** Performs test for 8bit wide sequential data without repeat. **/
  "checkSequentialOutputsWithoutRepeat" should "be ok" in {
    val data = 0 until 256
    Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/DecoupledDataSourceSuite/checkSequentialOutputWithoutRepeat"),
                   () => new DecoupledDataSource(UInt(8.W), 256, i => data(i).U, false))
      { m => new DecoupledDataSource_OutputCheck(m, data) }
  }
}
