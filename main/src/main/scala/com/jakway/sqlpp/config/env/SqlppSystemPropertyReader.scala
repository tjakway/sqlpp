package com.jakway.sqlpp.config.env

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.util.env.SystemPropertyReader

object SqlppSystemPropertyReader
  extends SystemPropertyReader[SqlppError]
    with SqlppSystemReader {
  override protected val actionDesc: Option[String] = Some("in System.getProperty")
}
