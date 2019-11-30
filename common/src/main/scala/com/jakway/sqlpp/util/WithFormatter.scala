package com.jakway.sqlpp.util

import java.util.Formatter

object WithFormatter {
  private def mkFormatter: Formatter = {
    val sb: Appendable = new StringBuffer()
    new Formatter(sb)
  }

  def apply(formatStr: String, args: Object*): String = {
    val fmt: Formatter = mkFormatter

    fmt.format(formatStr, args)
    fmt.toString
  }
}
