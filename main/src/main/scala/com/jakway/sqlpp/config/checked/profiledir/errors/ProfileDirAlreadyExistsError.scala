package com.jakway.sqlpp.config.checked.profiledir.errors

import java.io.File

class ProfileDirAlreadyExistsError(val location: File)
  extends CreateProfileDirFileOperationError(
    s"Could not create config directory at $location")
