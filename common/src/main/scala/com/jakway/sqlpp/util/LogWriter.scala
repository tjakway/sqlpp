package com.jakway.sqlpp.util

import java.io.Writer
import java.util.Formatter

import org.slf4j.Logger

class LogWriter(val logger: Logger,
                val logF: Logger => String => Unit,
                val formatString: String = LogWriter.defaultFormatString)
  extends Writer {

  private def getFmt: Formatter = {
    val sb = new StringBuffer()
    new Formatter(sb)
  }

  override def write(chars: Array[Char],
                     off: Int,
                     len: Int): Unit = {
    val sub = chars.subSequence(off, off + len)

    logF(logger)(getFmt.format(formatString, sub).toString)
  }

  override def flush(): Unit = {}

  override def close(): Unit = {}
}

object LogWriter {
  val defaultFormatString: String = "%s"
}
