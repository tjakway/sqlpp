package com.jakway.sqlpp

import java.io.InputStream
import java.util.Properties

import com.jakway.sqlpp.Config.ConfigError
import com.jakway.sqlpp.error.SqlppError

abstract class Backend(val names: Set[String]) {
  def matches(x: String): Boolean =
    names.exists(Backend.areEqual(_, x))

  def setEngineProperties(velocityProperties: Properties): Unit
}

abstract class NamedPropertiesBackend(override val names: Set[String])
  extends Backend(names) {

  override def setEngineProperties(velocityProperties: Properties): Unit = {

  }


}

object Backend {
  class BackendLoadError(override val causes: Seq[SqlppError])
    extends ConfigError(causes)

  class OpenStreamError(override val msg: String)
    extends SqlppError(msg)

  def areEqual(left: String, right: String): Boolean = {
    def normalizeString(str: String): String = str.trim.toLowerCase()

    normalizeString(left) == normalizeString(right)
  }

  case class ClassResourceBackend(override val names: Set[String],
                                  resourcePath: String)
    extends Backend(names) {

  }
}