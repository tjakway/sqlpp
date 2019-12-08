package com.jakway.sqlpp.config

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter, Writer}
import java.util.{Formatter, Locale}

import com.jakway.sqlpp.config.OutputString.{OutputStringFormatError, OutputStringFormatException, OutputStringOpenWriterError}
import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.error.{CheckFile, CheckString, SqlppError}
import com.jakway.sqlpp.util.TryToEither
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

class OutputString(val outputCharset: String)
                  (val formatStr: String) {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  private def substituteName(in: String): Either[SqlppError, String] =
    TryToEither(new OutputStringFormatException(_)) { Try {
      val fmt: Formatter = {
        val sb: Appendable = new StringBuffer()
        new Formatter(sb, Locale.getDefault)
      }

      fmt.format(formatStr, in)
      fmt.toString
    }
  }

  private def checkSubstitutedString(substituted: String):
    Either[SqlppError, String] = {
    for {
      _ <- CheckString.checkNonEmpty(
        new OutputStringFormatError(_))(substituted)
    } yield {
      substituted
    }
  }

  private def getWriter(f: File): Either[SqlppError, Writer] =
    TryToEither(new OutputStringOpenWriterError(_)) { Try {
      new BufferedWriter(
        new OutputStreamWriter(new FileOutputStream(f), outputCharset))
    }
  }

  private def checkFile(f: File): Either[SqlppError, Unit] = {
    if(f.exists()) {
      for {
        _ <- CheckFile.checkIsFile(f)
        _ <- CheckFile.setWritable(true)(f)
      } yield {{}}
    } else {
      logger.debug(s"$f does not exist, skipping checks")
      Right({})
    }
  }

  def apply(in: String): Either[SqlppError, Writer] = {
    for {
      substitutedString <- substituteName(in)
      _ <- checkSubstitutedString(substitutedString)
      outFile = new File(substitutedString)
      _ <- checkFile(outFile)
      res <- getWriter(outFile)
    } yield {
      res
    }
  }
}

object OutputString {
  class OutputStringError(override val msg: String)
    extends ConfigError(msg)

  class OutputStringOpenWriterError(val throwable: Throwable)
    extends OutputStringError(SqlppError.formatThrowableCause(throwable))

  class OutputStringFormatError(override val msg: String)
    extends OutputStringError(msg)

  class OutputStringFormatException(val throwable: Throwable)
    extends OutputStringFormatError(SqlppError.formatThrowableCause(throwable))
}
