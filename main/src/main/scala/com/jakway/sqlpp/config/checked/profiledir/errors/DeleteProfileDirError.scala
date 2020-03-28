package com.jakway.sqlpp.config.checked.profiledir.errors

import java.io.File

import com.jakway.sqlpp.error.SqlppError

class DeleteProfileDirError(val profileDir: File,
                            val cause: String,
                            val precedingError: SqlppError)
  extends CreateProfileDirError(s"Failed to delete profile dir" +
    s" $profileDir due to $cause while trying to clean up" +
    s" after main error $precedingError")
