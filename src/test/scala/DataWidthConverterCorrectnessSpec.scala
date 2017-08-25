package chisel.miscutils
import  chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import  org.scalacheck._, org.scalacheck.Prop._
import  org.scalatest.prop.Checkers
import  generators._

class DataWidthConverterCorrectnessSpec extends ChiselFlatSpec with Checkers {
  behavior of "DataWidthConverter"

  it should "preserve data integrity in arbitrary conversions" in
    check(forAll(bitWidthGen(64), Arbitrary.arbitrary[Boolean]) { case (inW, littleEndian) =>
      forAll(conversionWidthGen(inW)) { outW =>
        println("Testing bitwidth conversion from %d bits -> %d bits (%s)"
          .format(inW:Int, outW:Int, if (littleEndian) "little-endian" else "big-endian"))
        Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/DataWidthConverter"),
            () => new DataWidthConverterHarness(inW, outW, littleEndian))
          { m => new DataWidthConverterCorrectnessTester(m) }
      }
    }, minSuccessful(20))
}
