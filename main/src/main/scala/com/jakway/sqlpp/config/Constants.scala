package com.jakway.sqlpp.config

/**
 * for values not intended to be overridden
 */
object Constants {
  val programName: String = "sqlpp"
  val version: String = "0.1"

  /**
   * this environment variable will be prepended to the arg list
   * before parsing
   * see [[com.jakway.sqlpp.config.unchecked.GetEnvVariableArgs]]
   */
  val additionalArgsEnvVarName: String = "SQLPP_ARGS"

  val templatesDirName: String = "templates"

  object StandardBackendResources {
    type ResourceType = (String, String)

    private def prefix(resName: String): (String, String) =
      (resName, "/gen/" + resName)

    val postgres: ResourceType = prefix("postgres.xml")
    val h2: ResourceType = prefix("h2.xml")
    val defaults: ResourceType = prefix("defaults.xml")

    lazy val allResources: Set[ResourceType] = Set(
      postgres, h2, defaults
    )
  }
}
