package com.jakway.sqlpp.config.env

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.util.env.SystemEnvReader

object SqlppSystemEnvReader
  extends SystemEnvReader[SqlppError]
    with SqlppSystemReader {
  override protected val actionDesc: Option[String] = Some("in System.getenv")
}
