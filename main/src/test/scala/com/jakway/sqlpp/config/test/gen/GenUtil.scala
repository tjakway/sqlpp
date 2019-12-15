package com.jakway.sqlpp.config.test.gen

import org.scalacheck.{Arbitrary, Gen}

object GenUtil {
  def const[A](a: A): Gen[A] =
    Arbitrary.arbBool.arbitrary.map(_ => a)

  def randomlyIntersperseInSeq[A](genXs: Gen[Seq[A]],
                                 genX: Gen[A],
                                 min: Int,
                                 max: Int): Gen[Seq[A]] = {

    def insertIn(ys: Seq[A],
                 y: A,
                 at: Int): Seq[A] = {
      val (left, right) = ys.splitAt(at)
      (left :+ y ) ++ right
    }

    def genInsertPosition(ys: Seq[A]): Gen[Int] =
      Gen.choose(0, ys.length)

    Gen.chooseNum(min, max).flatMap { numToInsert =>
      val genToInsert: Gen[Seq[A]] =
        genX.map(x => Seq.fill(numToInsert)(x))

      genToInsert.flatMap { toInsert =>

        toInsert.foldLeft(genXs) {
          case (accGenXs, thisItem) => {
            for {
              foldGenXs <- accGenXs
              insertPosition <- genInsertPosition(foldGenXs)
            } yield {
              insertIn(foldGenXs, thisItem, insertPosition)
            }
          }
        }
      }
    }
  }
}
