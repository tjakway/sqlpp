package com.jakway.sqlpp.template

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.PropertiesBackend.PropertiesBackendError
import com.jakway.sqlpp.template.PropertiesFileBackend.PropertiesFileBackendError
import com.jakway.sqlpp.template.PropertiesResourceBackend.PropertiesResourceBackendError
import com.jakway.sqlpp.util.TryToEither
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

abstract class PropertiesBackend(override val names: Set[String])
  extends Backend(names) {

  def getPropertiesInputStream: Either[SqlppError, InputStream]

  override def toValueSource: Either[SqlppError, ValueSource] = {
    getPropertiesInputStream.flatMap(PropertySource.fromXML)
  }
}

object PropertiesBackend {
  class PropertiesBackendError(override val msg: String)
    extends SqlppError(msg)
}

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

/**
 * @param names
 * @param resource Note: resource should have a leading slash
 */
class PropertiesResourceBackend(override val names: Set[String],
                                val resource: String)
  extends PropertiesBackend(names) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def getPropertiesInputStream: Either[SqlppError, InputStream] = {
    logger.debug(s"Opening resource $resource for Backend")

    if(!resource.startsWith("/")) {
      logger.warn(s"Resource string < $resource > " +
        "passed to getClass.getResourceAsStream " +
        "should have a leading slash")
    }

    TryToEither(PropertiesResourceBackendError.apply) {
      Try(getClass.getResourceAsStream(resource))
    }
  }
}

object PropertiesResourceBackend {
  case class PropertiesResourceBackendError(throwable: Throwable)
    extends PropertiesBackendError(
      SqlppError.formatThrowableCause(throwable))
}
