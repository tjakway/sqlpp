package com.jakway.sqlpp.config.error

import com.jakway.sqlpp.error.SqlppError

class OutputTargetErrors(val causes: Seq[SqlppError])
  extends ConfigError(causes)
