package com.simshi.fp.chapter4

/**
  * Created by simshi on 11/10/16.
  */

object c4 {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * Exercise 2
    * Implement the variance function in terms of flatMap. If the mean of a
    * sequence is m, the variance is the mean of math.pow(x - m, 2) for each
    * element x in the sequence.
    */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap {
      m => mean(xs.map(x => math.pow(x - m, 2)))
    }
  }

  import java.util.regex._

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  /** EXERCISE 4
    * Re-implement bothMatch above in terms of this new function, to the extent
    * possible
    */
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
  Option.map2(mkMatcher(pat1), mkMatcher(pat2))((as, bs) => as(s) && bs(s))

  def main(args: Array[String]): Unit = {
    println("==== Chapter 4: Handling Errors without Exceptions")
    println("==== Option ====")
    val s1: Option[Int] = Some(1)
    val s_minus1: Option[Int] = Some(-1)
    val noneInt: Option[Int] = None
    val ss: Option[String] = Some("hello")
    val noneString: Option[String] = None
    println(s"s1.map(_ + 1) = ${s1.map(_ + 1)}")
    println(s"noneInt.map(_ + 1) = ${noneInt.map(_ + 1)}")
    println(s"ss.map(_.length) = ${ss.map(_.length)}")
    println(s"noneString.map(_.length) = ${noneString.map(_.length)}")
    println(s"s1.getOrElse(0) = ${s1.getOrElse(0)}")
    println(s"noneInt.getOrElse(0) = ${noneInt.getOrElse(0)}")
    println(s"s1.orElse(Some(0)) = ${s1.orElse(Some(0))}")
    println(s"noneInt.orElse(Some(0)) = ${noneInt.orElse(Some(0))}")

    def negative2None(a: Int): Option[Int] = if (a < 0) None else Some(a)
    println(s"s1.flatMap(negative2None) = ${s1.flatMap(negative2None)}")
    println(s"noneInt.flatMap(negative2None) = ${noneInt.flatMap(negative2None)}")
    println(s"s_minus1.flatMap(negative2None) = ${s_minus1.flatMap(negative2None)}")

    println(s"s1.filter(_ > 0) = ${s1.filter(_ > 0)}")
    println(s"s1.filter(_ < 0) = ${s1.filter(_ < 0)}")
    println(s"noneInt.filter(_ > 0) = ${noneInt.filter(_ > 0)}")
    println(s"variance(Seq()) = ${variance(Seq())}")
    println(s"variance(Seq(0.0)) = ${variance(Seq(0.0))}")
    println(s"variance(Seq(2.0, 4, 6)) = ${variance(Seq(2.0, 4, 6))}")

    val patIdent = "[0-9a-z]+"
    val patNumber = "[0-9]+"
    val patAlphabet = "[a-z]+"
    val aNumberString = "223"
    println(s"bothMatch_2(patIdent, patNumber, aNumberString) = ${bothMatch_2(patIdent, patNumber, aNumberString)}")
    println(s"bothMatch_2(patIdent, patAlphabet, aNumberString) = ${bothMatch_2(patIdent, patAlphabet, aNumberString)}")

    println(s"sequence(List[Option[Int]]()) = ${Option.sequence(List[Option[Int]]())}")
    println(s"sequence(List[Option[Int]](None)) = ${Option.sequence(List[Option[Int]](None))}")
    println(s"sequence(List[Option[Int]](Some(1), Some(2), Some(3))) = ${Option.sequence(List[Option[Int]](Some(1), Some(2), Some(3)))}")
    println(s"sequence(List[Option[Int]](Some(1), None, Some(3))) = ${Option.sequence(List[Option[Int]](Some(1), None, Some(3)))}")
    println(s"sequence_1(List[Option[Int]]()) = ${Option.sequence_1(List[Option[Int]]())}")
    println(s"sequence_1(List[Option[Int]](None)) = ${Option.sequence_1(List[Option[Int]](None))}")
    println(s"sequence_1(List[Option[Int]](Some(1), Some(2), Some(3))) = ${Option.sequence_1(List[Option[Int]](Some(1), Some(2), Some(3)))}")
    println(s"sequence_1(List[Option[Int]](Some(1), None, Some(3))) = ${Option.sequence_1(List[Option[Int]](Some(1), None, Some(3)))}")
    println(s"sequenceViaTraverse(List[Option[Int]]()) = ${Option.sequenceViaTraverse(List[Option[Int]]())}")
    println(s"sequenceViaTraverse(List[Option[Int]](None)) = ${Option.sequenceViaTraverse(List[Option[Int]](None))}")
    println(s"sequenceViaTraverse(List[Option[Int]](Some(1), Some(2), Some(3))) = ${Option.sequenceViaTraverse(List[Option[Int]](Some(1), Some(2), Some(3)))}")
    println(s"sequenceViaTraverse(List[Option[Int]](Some(1), None, Some(3))) = ${Option.sequenceViaTraverse(List[Option[Int]](Some(1), None, Some(3)))}")

    println("==== Either ====")
    type ErrorOrInt = Either[String, Int]
    val left0: ErrorOrInt = Left("error")
    val right0: ErrorOrInt = Right(0)
    val right1: ErrorOrInt = Right(1)
    println(s"""Left(\"error\").map(_ + 1) = ${left0.map(_ + 1)}""")
    println(s"Right(0).map(_ + 1) = ${right0.map(_ + 1)}")

    def asDivisor(d: Int): ErrorOrInt = if (d == 0) Left("divide by zero") else Right(100 / d)
    println(s"""Left(\"error\").flatMap(_ + 1) = ${left0.flatMap(asDivisor)}""")
    println(s"Right(0).flatMap(100/a) = ${right0.flatMap(asDivisor)}")
    println(s"Right(1).flatMap(100/a) = ${right1.flatMap(asDivisor)}")
    println(s"""Left(\"error\").orElse(left0) = ${left0.orElse(left0)}""")
    println(s"""Left(\"error\").orElse(right0) = ${left0.orElse(right0)}""")
    println(s"Right(0).orElse(left0) = ${right0.orElse(left0)}")
    println(s"Right(0).orElse(right1) = ${right0.orElse(right1)}")
    println(s"""Left(\"error\").map2(left0)(_ + _) = ${left0.map2(left0)(_ + _)}""")
    println(s"Right(0).map2(right1)(_ + _) = ${right0.map2(right1)(_ + _)}")
    println(s"Right(1).map2(right1)(_ + _) = ${right1.map2(right1)(_ + _)}")
    println(s"sequence(List[ErrorOrInt]()) = ${Either.sequence(List[ErrorOrInt]())}")
    println(s"sequence(List[ErrorOrInt](left0)) = ${Either.sequence(List[ErrorOrInt](left0))}")
    println(s"sequence(List[ErrorOrInt](right0, right1))) = ${Either.sequence(List[ErrorOrInt](right0, right1))}")
    println(s"sequence(List[ErrorOrInt](right0, left0, right1))) = ${Either.sequence(List[ErrorOrInt](right0, left0, right1))}")
    println(s"sequenceViaTraverse(List[ErrorOrInt]()) = ${Either.sequenceViaTraverse(List[ErrorOrInt]())}")
    println(s"sequenceViaTraverse(List[ErrorOrInt](left0)) = ${Either.sequenceViaTraverse(List[ErrorOrInt](left0))}")
    println(s"sequenceViaTraverse(List[ErrorOrInt](right0, right1))) = ${Either.sequenceViaTraverse(List[ErrorOrInt](right0, right1))}")
    println(s"sequenceViaTraverse(List[ErrorOrInt](right0, left0, right1))) = ${Either.sequenceViaTraverse(List[ErrorOrInt](right0, left0, right1))}")

    println("==== Validation ====")
    type FailureOrInt = Validation[String, Int]
    val f0: FailureOrInt = Failure(List("f0"))
    val f1: FailureOrInt = Failure(List("f1"))
    val s0: FailureOrInt = Success(0)
    val suc1: FailureOrInt = Success(1)
    println(s"""Failure(List(\"f0\")).map(_ + 1) = ${f0.map(_ + 1)}""")
    println(s"Success(0).map(_ + 1) = ${s0.map(_ + 1)}")
    println(s"""Failure(List(\"f0\")).orElse(f1) = ${f0.orElse(f1)}""")
    println(s"""Failure(List(\"f0\")).orElse(s0) = ${f0.orElse(s0)}""")
    println(s"Success(0).orElse(f0) = ${s0.orElse(f0)}")
    println(s"Success(0).orElse(s1) = ${s0.orElse(suc1)}")
    println(s"sequence(List[FailureOrInt]()) = ${Validation.sequence(List[FailureOrInt]())}")
    println(s"sequence(List[FailureOrInt](f0)) = ${Validation.sequence(List[FailureOrInt](f0))}")
    println(s"sequence(List[FailureOrInt](s0, s1))) = ${Validation.sequence(List[FailureOrInt](s0, suc1))}")
    println(s"sequence(List[FailureOrInt](s0, f0, s1, f1))) = ${Validation.sequence(List[FailureOrInt](s0, f0, suc1, f1))}")
    println(s"sequenceViaTraverse(List[FailureOrInt]()) = ${Validation.sequenceViaTraverse(List[FailureOrInt]())}")
    println(s"sequenceViaTraverse(List[FailureOrInt](f0)) = ${Validation.sequenceViaTraverse(List[FailureOrInt](f0))}")
    println(s"sequenceViaTraverse(List[FailureOrInt](s0, s1))) = ${Validation.sequenceViaTraverse(List[FailureOrInt](s0, suc1))}")
    println(s"sequenceViaTraverse(List[FailureOrInt](s0, f0, s1, f1))) = ${Validation.sequenceViaTraverse(List[FailureOrInt](s0, f0, suc1, f1))}")
    println(s"sequence_2(List[FailureOrInt]()) = ${Validation.sequenceViaTraverseR(List[FailureOrInt]())}")
    println(s"sequence_2(List[FailureOrInt](f0)) = ${Validation.sequenceViaTraverseR(List[FailureOrInt](f0))}")
    println(s"sequence_2(List[FailureOrInt](s0, s1))) = ${Validation.sequenceViaTraverseR(List[FailureOrInt](s0, suc1))}")
    println(s"sequence_2(List[FailureOrInt](s0, f0, s1, f1))) = ${Validation.sequenceViaTraverseR(List[FailureOrInt](s0, f0, suc1, f1))}")
  }
}
