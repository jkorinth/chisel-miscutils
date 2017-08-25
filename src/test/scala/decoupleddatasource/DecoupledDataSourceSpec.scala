package chisel.miscutils.decoupleddatasource
import  chisel.miscutils._
import  chisel3._
import  chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import  org.scalacheck._, org.scalacheck.Prop._
import  org.scalatest.prop.Checkers
import  scala.util.Random
import  generators._

/**
 * Tester class for DecoupledDataSource.
 * Automatically exhausts the internal data of the instance, checks that
 * each dequeued output matches the expected value. Also checks whether
 * or not the module wraps correctly (in case repeat is true) and whether
 * it respects ready/valid handshaking (by waiting random cycles with
 * ready pulled low after each element).
 **/
class OutputCheck[T <: UInt](m: DecoupledDataSource[T], data: Int => Int) extends PeekPokeTester(m) {
  reset(10)
  poke(m.io.out.ready, true)
  var i = 0
  while (peek(m.io.out.valid) > 0 && i <= m.size) {
    if (i >= m.size) {
      expect(m.repeat, "repeat is false, but index (%d) exceeds size(%d)".format(i, m.size))// :: errors
    } else {
      expect(m.io.out.bits, data(i), "output #%d: expected %d, found %d".format(i, data(i), peek(m.io.out.bits)))
      // wait for random time up to 10 cycles
      val wait = (scala.math.random * 10).toInt
      poke(m.io.out.ready, false)
      step(wait)
      poke(m.io.out.ready, true)
    }
    i += 1
    step(1)
  }
}

class DecoupledDataSourceSpec extends ChiselFlatSpec with Checkers {
  behavior of "DecoupledDataSource"

  it should "generate random outputs correctly" in
    check(forAll(bitWidthGen(64), dataSizeGen(1024), Arbitrary.arbitrary[Boolean]) { case (bw, sz, re) =>
      println("Testing DecoupledDataSource with %d entries of width %d %s"
        .format(bw:Int, sz:Int, if (re) "with repeat" else "without repeat"))
      val data = 0 until sz map (i => scala.util.Random.nextInt().abs % math.pow(2, bw:Int).toInt)
      Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/DecoupledDataSource"),
          () => new DecoupledDataSource[UInt](0.U((bw:Int).W), sz, data map (_.U), re))
        { m => new OutputCheck(m, data) }
    }, minSuccessful(25))
}
