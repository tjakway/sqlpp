package com.jakway.sqlpp.error

import java.util.Formatter

class SqlppError(val msg: String)
  extends RuntimeException(msg)

object SqlppError {
  /**
   * put each error on a separate tab-indented line
   * @param errors
   * @return
   */
  def formatErrors(errors: Seq[SqlppError]): String = {
    val sb: Appendable = new StringBuffer()
    val fmt: Formatter = new Formatter(sb)

    if(errors.nonEmpty) {
      fmt.format("Errors:\n")
      errors.foreach { thisError =>
        fmt.format("\t%s\n", thisError.toString)
      }
    }

    fmt.toString.trim
  }

  def formatThrowable(t: Throwable): String = {
    //TODO: print stack trace, etc.
    t.toString
  }
}