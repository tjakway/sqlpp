package com.jakway.sqlpp.config.entries

import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.config.output.OutputPattern
import com.jakway.sqlpp.error.{CheckString, SqlppError}

import scala.util.matching.Regex

class ParseOutputPattern(val encoding: String) {
  class ParseOutputPatternError(override val msg: String)
    extends ConfigError(msg)

  class BadFormatStringError(override val msg: String)
    extends ParseOutputPatternError(msg)

  object EmptyFormatStringError
    extends BadFormatStringError("Passed format string was empty")

  class NoFormatSymbolError(override val msg: String)
    extends BadFormatStringError(msg)

  def apply(s: String,
            requireFormatSymbol: Boolean): Either[SqlppError, OutputPattern] = {
    if(CheckString.isEmpty(s)) {
      Left(EmptyFormatStringError)
    } else {
      //make sure the argument contains a format symbol if it needs one
      if(requireFormatSymbol
        && ParseOutputPattern
            .outputStringRegex
            .findFirstIn(s).isEmpty) {

        Left(new NoFormatSymbolError(s"Expected an unescaped " +
          s"format symbol < %s > to appear" +
          s" at least once in output string $s "))
      } else {
        Right(new OutputPattern(encoding)(s))
      }
    }
  }
}

object ParseOutputPattern {
  private lazy val unescaped: String = "(?<!\\\\)"

  lazy val outputStringRegex: Regex = {
    val unescapedPercentSRegex: String = unescaped + "%s"
    val matchAnything: String = ".*"

    new Regex(matchAnything + unescapedPercentSRegex + matchAnything)
  }


  lazy val stdoutSpecialRegex: Regex = {
    //see https://stackoverflow.com/q/4068629/389943
    val unescapedDashRegex: String =
      unescaped + "[" + stdoutSpecialChar.toString + "]"

    val matchAnyWhitespace = """\s*"""

    new Regex(matchAnyWhitespace + unescapedDashRegex + matchAnyWhitespace)
  }

  val stdoutSpecialChar: Character = '-'
}
