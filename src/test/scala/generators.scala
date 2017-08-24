package chisel.miscutils
import  org.scalacheck._
import  SignalGenerator._
import  scala.language.implicitConversions

package object generators {
  final case class Limited[A](a: A, min: A, max: A)(implicit num: Numeric[A]) {
    import num._
    require (a >= min, s"$a must be >= $min")
    require (a <= max, s"$a must be <= $max")
  }
  implicit def limitedToA[A](l: Limited[A]): A = l.a

  def genLimited[A](min: A, max: A)(implicit num: Numeric[A], c: Gen.Choose[A]): Gen[Limited[A]] =
    Gen.choose(min, max) map (v => Limited.apply(v, min, max))

  implicit def shrinkLimited[A](l: Limited[A])(implicit num: Numeric[A]): Shrink[Limited[A]] = Shrink { l =>
    import num._
    if (l.a <= l.min) Stream.empty[Limited[A]] else Stream(Limited(l.a - num.one, l.min, l.max))
  }

  type BitWidth = Limited[Int]
  type DataSize = Limited[Int]

  def bitWidthGen(max: Int = 64): Gen[BitWidth] = genLimited(1, max)
  def dataSizeGen(max: Int = 1024): Gen[DataSize] = genLimited(1, max)

  def widthConversionGen(max: Int = 64): Gen[(BitWidth, BitWidth, Boolean)] = for {
    inWidth      <- bitWidthGen(max)
    outWidth     <- bitWidthGen(max)
    littleEndian <- Arbitrary.arbitrary[Boolean]
  } yield (inWidth, outWidth, littleEndian)

  def dataSourceGen(maxWidth: Int = 64, maxSize: Int = 1024): Gen[(BitWidth, DataSize, Boolean)] = for {
    bw <- bitWidthGen(maxWidth)
    ds <- dataSizeGen(maxSize)
    r  <- Arbitrary.arbitrary[Boolean]
  } yield (bw, ds, r)

  def signalGen(maxLength: Int = 15): Gen[Signal] = for {
    v <- Arbitrary.arbitrary[Boolean]
    p <- genLimited(2, maxLength)
  } yield Signal(v, p)

  def alternate(ss: Seq[Signal]): Seq[Signal] = ss match {
    case s0 +: s1 +: sr => s0 +: alternate(s1.copy(value = !s0.value) +: sr)
    case s +: sr => s +: sr
    case Seq() => Seq()
  }

  def waveformGen(maxLength: Int = 20): Gen[Waveform] = Gen.sized { n =>
    Gen.nonEmptyBuildableOf[Seq[Signal], Signal](signalGen()) map (ss => Waveform(alternate(ss)))
  }

  implicit def waveformShrink(waveform: Waveform): Shrink[Waveform] =
    Shrink { w => if (w.length <= 1) Stream.empty[Waveform] else Stream(w.drop(1)) }

  def signalGeneratorGen: Gen[(Waveform, Boolean)] = for {
    w <- waveformGen()
    i <- Arbitrary.arbitrary[Boolean]
  } yield (w, true)
}
