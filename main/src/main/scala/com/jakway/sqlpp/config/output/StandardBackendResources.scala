package com.jakway.sqlpp.config.output

import java.io.InputStream

import com.jakway.sqlpp.config.Constants
import com.jakway.sqlpp.error.SqlppError

import scala.util.{Try, Success, Failure}

object StandardBackendResources {
  class ReadStandardBackendResourceError(val throwable: Throwable)
    extends SqlppError(SqlppError.formatThrowableCause(throwable))

  def getInputStreams: Either[SqlppError, Set[InputStream]] = {
    Try {
      Constants
        .StandardBackendResources
        .allResources
        .map(getClass.getResourceAsStream)
    } match {
      case Success(inputStreams) => Right(inputStreams)
      case Failure(t) =>
        Left(new ReadStandardBackendResourceError(t))
    }
  }
}
