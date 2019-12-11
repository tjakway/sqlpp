package com.jakway.sqlpp.config.entries

import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.config.output.{OutputPattern, StdoutOutputPattern}
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

  private def regexMatches(r: Regex, s: String): Boolean = {
    r.findFirstIn(s).isDefined
  }

  def apply(s: String,
            requireFormatSymbol: Boolean): Either[SqlppError, OutputPattern] = {
    def standardOutputPattern =
      Right(new OutputPattern(encoding)(s))

    def stdoutOutputPattern =
      Right(new StdoutOutputPattern(encoding))

    //always reject empty strings
    if(CheckString.isNonEmpty(s)) {
      //handle the "write to stdout" regex, i.e. the dash
      if(regexMatches(ParseOutputPattern.stdoutSpecialRegex, s)) {
        stdoutOutputPattern
      } else {
        //make sure the argument contains a format symbol if it needs one
        if(requireFormatSymbol) {
          if(regexMatches(ParseOutputPattern.outputStringRegex, s)) {
            standardOutputPattern
          } else {
            Left(new NoFormatSymbolError(s"Expected an unescaped " +
              s"format symbol < %s > to appear" +
              s" at least once in output string $s "))
          }
        } else {
          standardOutputPattern
        }
      }
    } else {
      Left(EmptyFormatStringError)
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
