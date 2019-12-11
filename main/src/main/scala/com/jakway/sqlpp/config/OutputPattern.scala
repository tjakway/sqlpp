package com.jakway.sqlpp.config

import java.io._
import java.util.{Formatter, Locale}

import com.jakway.sqlpp.config.OutputPattern.{OutputPatternError, OutputPatternFormatError, OutputPatternFormatException, OutputPatternOpenWriterError}
import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.error.{CheckFile, CheckString, SqlppError}
import com.jakway.sqlpp.template.Backend
import com.jakway.sqlpp.util.{FileUtil, TryClose, TryToEither}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

class OutputPattern(val outputCharset: String)
                   (val formatStr: String) {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  private def substituteName(in: String): Either[SqlppError, String] =
    TryToEither(new OutputPatternFormatException(_)) { Try {
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
        new OutputPatternFormatError(_))(substituted)
    } yield {
      substituted
    }
  }

  private def getWriter(f: File): Either[SqlppError, Writer] =
    TryToEither(new OutputPatternOpenWriterError(_)) { Try {
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

  protected def apply(in: String): Either[SqlppError, Writer] = {
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

  protected def apply(backend: Backend): Either[SqlppError, Writer] = {
    backend.toOutputStringName.flatMap(apply)
  }

  def permutateBackendDests(backends: Seq[Backend]):
    Either[SqlppError, Map[Backend, Writer]] = {

    val empty: Either[SqlppError, Map[Backend, Writer]] =
      Right(Map.empty)

    backends.foldLeft(empty) {
      case (eAcc, thisBackend) => eAcc.flatMap { acc =>
        apply(thisBackend) match {
          case Right(outWriter) =>
            Right(acc.updated(thisBackend, outWriter))

            //on error, close the open writers before returning
          case Left(e) => {
            acc.foreach {
              case (backend, writer) =>
                TryClose(writer, Some(s"writer for backend $backend"))
            }

            Left(e)
          }
        }
      }
    }
  }
}

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

object OutputPattern {
  class OutputPatternError(override val msg: String)
    extends ConfigError(msg)

  class OutputPatternOpenWriterError(val throwable: Throwable)
    extends OutputPatternError(SqlppError.formatThrowableCause(throwable))

  class OutputPatternFormatError(override val msg: String)
    extends OutputPatternError(msg)

  class OutputPatternFormatException(val throwable: Throwable)
    extends OutputPatternFormatError(SqlppError.formatThrowableCause(throwable))
}
