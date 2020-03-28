package com.jakway.sqlpp.config.env

import java.io.File

import com.jakway.sqlpp.error.SqlppError

object UserHome {
  val homeVarName: String = "HOME"

  def get: Either[SqlppError, File] = {
    SqlppSystemEnvReader.getPropertyF(homeVarName)
      .map(new File(_))
  }
}
