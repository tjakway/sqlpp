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
}
