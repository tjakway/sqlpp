package com.jakway.sqlpp.config.test.gen

import java.io.File

import org.scalacheck.Gen

trait GenTemplate {
  def templateKeys: Set[String]

  val genTemplate: Gen[String] =
    Gen.listOf(genSentence).map(_.mkString)

  lazy val genWord: Gen[String] = Gen.lzy {
    Gen.oneOf(Gen.alphaNumStr, Gen.oneOf(templateKeys))
  }

  lazy val genSeparator: Gen[String] = {
    //gen a list of whitespace chars
    val res = Gen.option(
      Gen.listOf(
        Gen.oneOf(
          GenUtil.whitespaceChars, System.getProperty("line.separator"))))

    res.map {
      case Some(list) => {
        //concatenate strings
        list.foldLeft("") {
          case (acc, i) => acc + i
        }
      }
      case None => ""
    }
  }

  lazy val genWordSeparatorPair: Gen[(String, String)] = {
    genWord.flatMap { word =>
      genSeparator.map(sep => (word, sep))
    }
  }

  lazy val genWordSeparatorPairs: Gen[Seq[(String, String)]] =
    Gen.listOf(genWordSeparatorPair)

  lazy val genSentence: Gen[String] = {
    genSeparator.flatMap { optionalLeadingWhitespace =>
      genSeparator.flatMap { optionalTrailingWhitespace =>
        genWordSeparatorPairs.map { wordSeparatorPairs =>
          //join all the words and separators together
          val start: String = optionalLeadingWhitespace
          val res = wordSeparatorPairs.foldLeft(start) {
            case (acc, (word, sep)) => acc + word + sep
          }

          res + optionalTrailingWhitespace
        }
      }
    }
  }
}
