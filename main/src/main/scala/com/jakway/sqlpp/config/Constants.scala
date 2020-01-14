package com.jakway.sqlpp.config

/**
 * for values not intended to be overridden
 */
object Constants {

  /**
   * this environment variable will be prepended to the arg list
   * before parsing
   * see [[com.jakway.sqlpp.config.unchecked.GetEnvVariableArgs]]
   */
  val additionalArgsEnvVarName: String = "SQLPP_ARGS"

  val templatesDirName: String = "templates"

  object StandardBackendResources {
    private def prefix(resName: String): String = "/gen/" + resName

    val postgres: String = prefix("postgres.xml")
    val h2: String = prefix("h2.xml")
    val defaults: String = prefix("defaults.xml")
  }
}
