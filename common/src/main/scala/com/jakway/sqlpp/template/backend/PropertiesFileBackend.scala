package com.jakway.sqlpp.template.backend

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.backend.PropertiesBackend.PropertiesBackendError
import com.jakway.sqlpp.template.backend.PropertiesFileBackend.PropertiesFileBackendError
import com.jakway.sqlpp.util.TryToEither
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

class PropertiesFileBackend(override val names: Set[String],
                            val propertiesFile: File)
  extends PropertiesBackend(names) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def getPropertiesInputStream: Either[SqlppError, InputStream] = {
    logger.debug(s"Opening file $propertiesFile for Backend")

    TryToEither(PropertiesFileBackendError.apply) {
      Try(new BufferedInputStream(new FileInputStream(propertiesFile)))
    }
  }
}

object PropertiesFileBackend {
  case class PropertiesFileBackendError(throwable: Throwable)
    extends PropertiesBackendError(
      SqlppError.formatThrowableCause(throwable))
}
