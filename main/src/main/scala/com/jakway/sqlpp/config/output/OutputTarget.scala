package com.jakway.sqlpp.config.output

import java.io.{BufferedWriter, File, OutputStreamWriter, Writer}

import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.config.output.OutputTarget.OpenWriterError
import com.jakway.sqlpp.error
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.{TemplateEngine, ValueSource}
import com.jakway.sqlpp.template.backend.Backend
import com.jakway.sqlpp.util.FileUtil

import scala.util.{Failure, Success, Try}

abstract class OutputTarget(val backend: Backend) {
  def getWriter(encoding: String): Either[SqlppError, Writer]
}

object OutputTarget {
  /**
   * @param outputTargets
   * @param outputEncoding
   * @return an IOMap suitable for passing to TemplateEngine
   */
  def toIOMap(outputTargets: Seq[OutputTarget], outputEncoding: String):
    Either[SqlppError, TemplateEngine.IOMap] = {

    val empty: Either[SqlppError, TemplateEngine.IOMap] =
      Right(Map.empty)

    outputTargets.foldLeft(empty) {
      case (eAcc, thisOutputTarget) => eAcc.flatMap { acc =>
        thisOutputTarget
          .getWriter(outputEncoding)
          .map(writer =>
            acc.updated(thisOutputTarget.backend, writer))

      }
    }
  }

  class OutputTargetError(override val msg: String)
    extends ConfigError(msg)

  class OpenWriterError(override val msg: String)
    extends OutputTargetError(msg) {
    def this(throwable: Throwable) {
      this(SqlppError.formatThrowable(throwable))
    }
  }

  object OpenWriterError {
    def fromTry(f: => Try[Writer]): Either[SqlppError, Writer] = {
      f match {
        case Success(w) => Right(w)
        case Failure(t) => Left(new OpenWriterError(t))
      }
    }
  }
}

class FileOutputTarget(override val backend: Backend,
                       val dest: File)
  extends OutputTarget(backend) {

  override def getWriter(encoding: String): Either[SqlppError, Writer] =
    FileUtil.openWriter(encoding, new OpenWriterError(_))(dest)
}

class StdoutOutputTarget(override val backend: Backend)
  extends OutputTarget(backend) {

  override def getWriter(encoding: String): Either[SqlppError, Writer] =
    OpenWriterError.fromTry(Try(
      new BufferedWriter(new OutputStreamWriter(System.out))))
}
