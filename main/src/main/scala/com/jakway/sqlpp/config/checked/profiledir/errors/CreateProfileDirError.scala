package com.jakway.sqlpp.config.checked.profiledir.errors

import com.jakway.sqlpp.error.SqlppError

class CreateProfileDirError(override val msg: String)
  extends SqlppError(msg)

