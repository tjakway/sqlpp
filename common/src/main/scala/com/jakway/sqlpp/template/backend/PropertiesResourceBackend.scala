package com.jakway.sqlpp.template.backend

import java.io.InputStream

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.backend.PropertiesBackend.PropertiesBackendError
import com.jakway.sqlpp.template.backend.PropertiesResourceBackend.PropertiesResourceBackendError
import com.jakway.sqlpp.util.TryToEither
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

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

  override def toString: String = {
    getClass.getSimpleName + "(" + resource + ")"
  }
}

object PropertiesResourceBackend {
  case class PropertiesResourceBackendError(throwable: Throwable)
    extends PropertiesBackendError(
      SqlppError.formatThrowableCause(throwable))
}
