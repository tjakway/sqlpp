package com.jakway.sqlpp.error

import java.io.{PrintWriter, StringWriter}
import java.util.Formatter

class SqlppError(val msg: String)
  extends RuntimeException(msg) {
  def print: String = toString

  def stackTraceToString: String = SqlppError.stackTraceToString(this)
}

object SqlppError {
  def stackTraceToString(throwable: Throwable): String = {
    //see https://stackoverflow.com/questions/1149703/how-can-i-convert-a-stack-trace-to-a-string

    val sw = new StringWriter()
    val pw = new PrintWriter(sw)

    throwable.printStackTrace(pw)
    sw.toString
  }

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
    stackTraceToString(t)
  }

  def formatThrowableCause(t: Throwable): String = {
    "Caused by " + stackTraceToString(t)
  }
}