package com.jakway.sqlpp.config.entries

import com.jakway.sqlpp.config.OutputString
import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.error.{CheckString, SqlppError}

import scala.util.matching.Regex

class ParseOutputString(val encoding: String) {
  lazy val outputStringRegex: Regex = {
    val unescapedPercentSRegex: String = "(?<!\\\\)%s"
    val matchAnything: String = ".*"

    new Regex(matchAnything + unescapedPercentSRegex + matchAnything)
  }

  class ParseOutputStringError(override val msg: String)
    extends ConfigError(msg)

  class BadFormatStringError(override val msg: String)
    extends ConfigError(msg)

  object EmptyFormatStringError
    extends BadFormatStringError("Passed format string was empty")

  class NoFormatSymbolError(override val msg: String)
    extends BadFormatStringError(msg)

  def apply(s: String,
            requireFormatSymbol: Boolean): Either[SqlppError, OutputString] = {
    if(CheckString.isEmpty(s)) {
      Left(EmptyFormatStringError)
    } else {
      //make sure the argument contains a format symbol if it needs one
      if(requireFormatSymbol
        && outputStringRegex.findFirstIn(s).isEmpty) {

        Left(new NoFormatSymbolError(s"Expected an unescaped " +
          s"format symbol < %s > to appear" +
          s" at least once in output string $s "))
      } else {
        Right(new OutputString(encoding)(s))
      }
    }
  }
}