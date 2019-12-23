package com.jakway.sqlpp.template.backend

import java.io.InputStream

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.{PropertySource, ValueSource}

abstract class PropertiesBackend(override val names: Set[String])
  extends Backend(names) {

  def getPropertiesInputStream: Either[SqlppError, InputStream]

  override protected def getValueSource: Either[SqlppError, ValueSource] = {
    getPropertiesInputStream.flatMap(PropertySource.fromXML)
  }
}

object PropertiesBackend {
  class PropertiesBackendError(override val msg: String)
    extends SqlppError(msg)
}
