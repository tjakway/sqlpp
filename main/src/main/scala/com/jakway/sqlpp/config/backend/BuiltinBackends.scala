package com.jakway.sqlpp.config.backend

import com.jakway.sqlpp.config.Constants
import com.jakway.sqlpp.template.backend.PropertiesResourceBackend

object BuiltinBackends {
  import Constants.StandardBackendResources

  private def fromResourceType:
    StandardBackendResources.ResourceType =>
    PropertiesResourceBackend = {
    (resTuple: StandardBackendResources.ResourceType) =>

      val (name, resource) = resTuple
      new PropertiesResourceBackend(Set(name), resource)
  }

  lazy val postgres: PropertiesResourceBackend =
    fromResourceType(StandardBackendResources.postgres)

  lazy val h2: PropertiesResourceBackend =
    fromResourceType(StandardBackendResources.h2)

  lazy val defaults: PropertiesResourceBackend =
    fromResourceType(StandardBackendResources.defaults)

  lazy val all: Set[PropertiesResourceBackend] = Set(
    postgres, h2, defaults
  )
}
