package com.jakway.sqlpp.config.output

import java.io.InputStream

import com.jakway.sqlpp.config.Constants
import com.jakway.sqlpp.error.SqlppError

import scala.util.{Try, Success, Failure}

object StandardBackendResources {
  class ReadStandardBackendResourceError(val throwable: Throwable)
    extends SqlppError(SqlppError.formatThrowableCause(throwable))

  def getInputStreamPairs: Either[SqlppError, Set[(String, InputStream)]] = {
    Try {
      Constants
        .StandardBackendResources
        .allResources
        .map(xs => (xs._1, getClass.getResourceAsStream(xs._2)))
    } match {
      case Success(x) => Right(x)
      case Failure(t) =>
        Left(new ReadStandardBackendResourceError(t))
    }
  }
}
