package com.jakway.sqlpp.config.checked.profiledir

import java.io.File

import com.jakway.sqlpp.config.{Defaults, PrioritizedPref}
import com.jakway.sqlpp.config.env.{SqlppSystemEnvReader, SqlppSystemPropertyReader, XdgConfigHome}
import com.jakway.sqlpp.error.SqlppError

object GetDefaultProfileDirLocation
  extends PrioritizedPref[File](GetDefaultProfileDirGetters.orderedGetters)


private object GetDefaultProfileDirGetters {
  def orderedGetters: PrioritizedPref.OrderedGetters[File] = {
    Seq(
      () => getConfigDirEnvValue,
      () => getConfigDirPropertyValue,
      () => getXdgConfigDir
    )
  }

  def getXdgConfigDir: Either[SqlppError, File] = {
    XdgConfigHome
      .get
      .map(xdgConfigHome =>
        new File(xdgConfigHome, Defaults.DataDir.homeSubdirName))
  }

  private def getConfigDirValue:
    (String => Either[SqlppError, String]) =>
    Either[SqlppError, File] = {
    (getPropertyF: (String => Either[SqlppError, String])) =>
      getPropertyF(Defaults.DataDir.dataDirVarName)
        .map(new File(_))

  }

  def getConfigDirEnvValue: Either[SqlppError, File] = {
    getConfigDirValue(SqlppSystemEnvReader.getPropertyF)
  }

  def getConfigDirPropertyValue: Either[SqlppError, File] = {
    getConfigDirValue(SqlppSystemPropertyReader.getPropertyF)
  }
}

