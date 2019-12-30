package com.jakway.sqlpp.config.test

import com.jakway.sqlpp.config.entries.ParseOutputPattern
import com.jakway.sqlpp.config.output.StdoutOutputPattern
import com.jakway.sqlpp.config.test.gen.GenUtil
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class ParseOutputPatternProperties
  extends AnyPropSpec
    with Matchers
    with WithDefaultTestConfig {
  import ScalaCheckPropertyChecks._

  property("Parse dash to indicate write to stdout") {
    forAll(ParseOutputPatternProperties
      .GenParseOutputPatternTest
      .genParseDashTest(GenTestConfig.genEncoding)) { test =>

      val res = new ParseOutputPattern(test.encoding)
        .apply(test.toParse, test.requireFormatSymbol)

      res.map(x => x.isInstanceOf[StdoutOutputPattern]) shouldEqual
        Right(true)
    }
  }
}

object ParseOutputPatternProperties {
  class ParseOutputPatternTest(val encoding: String,
                               val requireFormatSymbol: Boolean,
                               val toParse: String)

  object GenParseOutputPatternTest {
    private val defaultGenRequireFormatSymbol: Gen[Boolean] =
      Arbitrary.arbBool.arbitrary

    private def apply(genEncoding: Gen[String],
                      genToParse: Gen[String],
                      genRequireFormatSymbol: Gen[Boolean] =
                        defaultGenRequireFormatSymbol): Gen[ParseOutputPatternTest] = {
      genEncoding.flatMap { encoding =>
        genRequireFormatSymbol.flatMap { requireFormatSymbol =>
          genToParse.map { toParse =>
            new ParseOutputPatternTest(encoding, requireFormatSymbol, toParse)
          }
        }
      }
    }

    private val genOptionalWhitespace: Gen[Seq[Char]] = {
      Gen.someOf(GenUtil.whitespaceChars)
    }

    private val genDashString: Gen[String] = {
      val res: Gen[Seq[Char]] =
        GenUtil.randomlyIntersperseInSeq(
          genOptionalWhitespace,
          Gen.const(ParseOutputPattern.stdoutSpecialChar),
          1, 1)
      res.map(_.mkString)
    }

    def genParseDashTest(genEncoding: Gen[String]): Gen[ParseOutputPatternTest] = {
      apply(genEncoding, genDashString, GenUtil.const(false))
    }
  }
}
