package com.jakway.sqlpp.config.error

import com.jakway.sqlpp.error.SqlppError

class ConfigError(override val msg: String)
  extends SqlppError(msg) {

  def this(causes: Seq[SqlppError]) {
    this(SqlppError.formatErrors(causes))
  }
}
