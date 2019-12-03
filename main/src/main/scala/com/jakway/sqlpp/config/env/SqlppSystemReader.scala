package com.jakway.sqlpp.config.env

import com.jakway.sqlpp.config.Config.ConfigError
import com.jakway.sqlpp.config.env.SqlppSystemReader.Error.{SqlppSystemReaderError, SqlppSystemReaderInvalidKeyError, SqlppSystemReaderKeyNotFoundError}
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.util.env.SystemValuesReader

trait SqlppSystemReader
  extends SystemValuesReader[SqlppError] {
  type ValueType = String
  type ErrorType = SqlppError

  override protected def onKeyNotFound: String => Either[ErrorType, ValueType] =
    e => Left(SqlppSystemReaderKeyNotFoundError.apply(e))

  override protected def onInvalidKey: String => Either[ErrorType, ValueType] =
    e => Left(SqlppSystemReaderInvalidKeyError.apply(e))

  override protected def onOtherError: String => Either[ErrorType, ValueType] =
    e => Left(new SqlppSystemReaderError(e))
}

object SqlppSystemReader {
  object Error {
    class SqlppSystemReaderError(override val msg: String)
      extends ConfigError(msg)

    class SqlppSystemReaderKeyError(override val msg: String)
      extends SqlppSystemReaderError(msg)

    case class SqlppSystemReaderKeyNotFoundError(override val msg: String)
      extends SqlppSystemReaderKeyError(msg)

    case class SqlppSystemReaderInvalidKeyError(override val msg: String)
      extends SqlppSystemReaderKeyError(msg)
  }
}

