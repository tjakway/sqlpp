package com.jakway.sqlpp.config.checked.profiledir.errors

import com.jakway.sqlpp.error.SqlppError

class CreateProfileDirFileOperationError(override val msg: String)
  extends CreateProfileDirError(msg)

object CreateProfileDirFileOperationError {
  def apply(throwable: Throwable): CreateProfileDirFileOperationError =
    new CreateProfileDirFileOperationError(
      SqlppError.formatThrowableCause(throwable))
}