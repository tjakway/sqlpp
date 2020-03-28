package com.jakway.sqlpp.config.env

import java.io.File

import com.jakway.sqlpp.error.SqlppError
import org.slf4j.{Logger, LoggerFactory}

object XdgConfigHome {
  val xdgConfigHomeVarName: String = "XDG_CONFIG_HOME"

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private def getDefaultXdgConfigHome: Either[SqlppError, File] = {
    UserHome.get.map(homeDir => new File(homeDir, ".config"))
  }

  def get: Either[SqlppError, File] = {
    SqlppSystemEnvReader
      .getPropertyF(xdgConfigHomeVarName)
      .map(new File(_)) match {
      case Right(x) => Right(x)
      case e@Left(_) => {
        logger.warn(s"Could not read environment variable " +
          s"$xdgConfigHomeVarName because of error $e," +
          s" attempting to fall back to default")

        getDefaultXdgConfigHome
      }
    }
  }
}
