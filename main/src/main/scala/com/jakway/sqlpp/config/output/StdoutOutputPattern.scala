package com.jakway.sqlpp.config.output

import java.io.Writer

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.Backend
import com.jakway.sqlpp.util.FileUtil

class StdoutOutputPattern(override val outputCharset: String)
  extends OutputPattern(outputCharset)(StdoutOutputPattern.dashPattern) {
  import StdoutOutputPattern.StdoutOutputPatternOpenWriterError

  protected def getStdoutWriter: Either[SqlppError, Writer] =
    FileUtil.openPrintStreamWriter[SqlppError](
      outputCharset,
      new StdoutOutputPatternOpenWriterError(_))(
      System.out)

  override protected def apply(in: String): Either[SqlppError, Writer] = {
    getStdoutWriter
  }

  override protected def apply(backend: Backend): Either[SqlppError, Writer] = {
    getStdoutWriter
  }
}

object StdoutOutputPattern {
  val dashPattern = "-"
  class StdoutOutputPatternOpenWriterError(override val throwable: Throwable)
    extends OutputPattern.OutputPatternOpenWriterError(throwable)
}

