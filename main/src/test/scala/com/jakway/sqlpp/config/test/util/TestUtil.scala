package com.jakway.sqlpp.config.test.util

import java.io.InputStream

import com.jakway.sqlpp.config.test.TestResources.OpenTestResourceError
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.util.TryToEither

import scala.util.Try

trait TestUtil {
  def openTestResource(loc: String): Either[SqlppError, InputStream] = {
    TryToEither(new OpenTestResourceError(_)) {
      Try(getClass.getResourceAsStream(loc))
    }
  }
}

object TestUtil extends TestUtil
