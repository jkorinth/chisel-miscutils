package chisel.miscutils.datawidthconverter
import  chisel.miscutils._
import  chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import  org.scalacheck._, org.scalacheck.Prop._
import  org.scalatest.prop.Checkers
import  generators._

class DataWidthConverterSpec extends ChiselFlatSpec with Checkers {
  behavior of "DataWidthConverter"

  it should "preserve data integrity in arbitrary conversions" in
    check(forAll(bitWidthGen(64), Arbitrary.arbitrary[Boolean], genLimited(1, 100)) {
      case (inW, littleEndian, delay) =>
        forAll(conversionWidthGen(inW)) { outW =>
          println("Testing bitwidth conversion from %d bits -> %d bits (%s) with %d delay"
            .format(inW:Int, outW:Int, if (littleEndian) "little-endian" else "big-endian", delay:Int))
          Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/DataWidthConverter"),
              () => new CorrectnessHarness(inW, outW, littleEndian, delay))
            { m => new CorrectnessTester(m) }
        }
      })

  it should "transfer data with minimal delays" in
    check(forAll(bitWidthGen(64), Arbitrary.arbitrary[Boolean]) { case (inW, littleEndian) =>
      forAll(conversionWidthGen(inW)) { outW =>
        println("Testing bitwidth conversion from %d bits -> %d bits (%s)"
          .format(inW:Int, outW:Int, if (littleEndian) "little-endian" else "big-endian"))
        Driver.execute(Array("--fint-write-vcd", "--target-dir", "test/DataWidthConverter"),
            () => new MinimalDelayHarness(inW, outW, littleEndian))
          { m => new MinimalDelayTester(m) }
      }
    })
}
