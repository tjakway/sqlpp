package com.jakway.sqlpp.config.output

import java.io.File

import com.jakway.sqlpp.config.Constants
import com.jakway.sqlpp.template.backend.{Backend, PropertiesFileBackend, PropertiesResourceBackend}

class StandardBackends(val postgres: Backend,
                       val h2: Backend,
                       val defaults: Backend)

object StandardBackends {
  def apply(configDir: Option[File]): StandardBackends = {
    lazy val templateDir: Option[File] =
    configDir.map(new File(_, Constants.templatesDirName))

    def loadBackend(name: String,
                    resource: String): Backend = {

      val optPropertiesFile: Option[File] =
        templateDir.map(new File(_, name + ".xml"))

      optPropertiesFile match {
        case Some(propertiesFile) =>
          new PropertiesFileBackend(Set(name), propertiesFile)

        case None => new PropertiesResourceBackend(Set(name), resource)
      }
    }


    import Constants.StandardBackendResources
    new StandardBackends(
      loadBackend("postgres", StandardBackendResources.postgres),
      loadBackend("h2", StandardBackendResources.h2),
      loadBackend("defaults", StandardBackendResources.defaults))
  }
}

