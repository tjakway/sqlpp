package com.jakway.sqlpp.config.test

import com.jakway.sqlpp.config.entries.ParseOutputPattern
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class ParseOutputPatternProperties
  extends AnyPropSpec
    with Matchers {
  import ScalaCheckPropertyChecks._

  property("Parse dash to indicate write to stdout") {
    forAll(ParseOutputPatternProperties
      .GenParseOutputPatternTest
      .genParseDashTest) { test =>
      true
    }
  }
}

object ParseOutputPatternProperties {
  class ParseOutputPatternTest(val encoding: String,
                               val requireFormatSymbol: Boolean,
                               val toParse: String)

  object GenParseOutputPatternTest {
    private def apply(genToParse: Gen[String]): Gen[ParseOutputPatternTest] = {
      TestConfig.genEncoding.flatMap { encoding =>
        Arbitrary.arbBool.arbitrary.flatMap { requireFormatSymbol =>
          genToParse.map { toParse =>
            new ParseOutputPatternTest(encoding, requireFormatSymbol, toParse)
          }
        }
      }
    }

    private val genOptionalWhitespace: Gen[Seq[Char]] = {
      //there's some ambiguity here, but try our best
      val whitespaceChars: Set[Char] =
        Set('\t', '\n', '\r', '\f', ' ')
            .filter(Character.isWhitespace)

      Gen.someOf(whitespaceChars)
    }

    private val genDashString: Gen[String] = {
      val res: Gen[Seq[Char]] =
        GenUtil.randomlyIntersperseInSeq(
          genOptionalWhitespace,
          Gen.const(ParseOutputPattern.stdoutSpecialChar),
          1, 1)
      res.map(_.mkString)
    }

    val genParseDashTest: Gen[ParseOutputPatternTest] = {
      apply(genDashString)
    }
  }
}
