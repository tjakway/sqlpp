package com.jakway.sqlpp.util

import java.io.File
import java.util.Formatter

import org.slf4j.{Logger, LoggerFactory}

import scala.util.matching.Regex

object WithFormatter {
  private def mkFormatter: Formatter = {
    val sb: Appendable = new StringBuffer()
    new Formatter(sb)
  }

  def apply(formatStr: String, args: Object*): String = {
    val fmt: Formatter = mkFormatter

    fmt.format(formatStr, args: _*)
    fmt.toString
  }

  object WithPathSeparator {
    private val formatStrRegex: Regex = """(?<!\\\\)%s""".r

    //see https://stackoverflow.com/questions/5971964/file-separator-or-file-pathseparator
    //regarding which field to use
    val defaultPathSeparator: String = File.separator

    private def countFormatStrings(str: String): Int = {
      formatStrRegex.findAllIn(str).length
    }

    def apply(formatStr: String,
              useSeparator: String = defaultPathSeparator): String = {

      //count the number of times %s appears in the formatStr
      //then make a Seq of that length
      val replaceArgs: Seq[String] =
        List.fill(countFormatStrings(formatStr))(useSeparator)

      WithFormatter.apply(formatStr, replaceArgs: _*)
    }
  }
}
