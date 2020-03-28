package com.jakway.sqlpp.template.backend

import java.io.{File, InputStream}

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.backend.PropertiesBackend.PropertiesBackendError
import com.jakway.sqlpp.template.{PropertySource, ValueSource}
import com.jakway.sqlpp.util.TryToEither
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

abstract class PropertiesBackend(override val names: Set[String])
  extends Backend(names) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def getPropertiesInputStream: Either[SqlppError, InputStream]

  override def read: Either[SqlppError, InputStream] = getPropertiesInputStream

  override def getValueSource(): Either[SqlppError, ValueSource] = {
    getPropertiesInputStream.flatMap { is =>
      val res = Try(PropertySource.fromXML(is))

      //close the InputStream, regardless of outcome
      try {
        Option(is).foreach { x =>
          x.close()
          logger.debug(s"Closed InputStream for $toString")
        }
      } catch {
        case t: Throwable => logger.error(
          s"Error closing InputStream for $toString", t)
      }

      def onError: Throwable => SqlppError = new PropertiesBackendError(_)

      TryToEither.handleNested(onError)(res)
    }
  }

  protected def extension: String = ".xml"

  private def addExtension(ext: String, to: String): String = {
    if(to.endsWith(ext)) {
      to
    } else {
      to + ext
    }
  }

  override def getFileInDir: File => Either[SqlppError, File] = { (dir: File) =>
    getName.map(name => new File(dir, addExtension(extension, name)))
  }
}

object PropertiesBackend {
  class PropertiesBackendError(override val msg: String)
      extends SqlppError(msg) {
    def this(throwable: Throwable) {
      this(SqlppError.formatThrowableCause(throwable))
    }
  }
}
