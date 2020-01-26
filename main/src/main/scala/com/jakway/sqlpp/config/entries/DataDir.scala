package com.jakway.sqlpp.config.entries

import java.io.File

import com.jakway.sqlpp.config
import com.jakway.sqlpp.config.PrioritizedPref
import com.jakway.sqlpp.config.env.{SqlppSystemEnvReader, SqlppSystemPropertyReader, UserHome, XdgConfigHome}
import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import org.slf4j.{Logger, LoggerFactory}

object DataDir {
  type CreateDirF = File => Either[SqlppError, Unit]
  import com.jakway.sqlpp.config.Defaults.DataDir._

  class DataDirError(override val msg: String)
    extends ConfigError(msg)

  private def readDataDirEnvVariable: Either[SqlppError, File] =
    SqlppSystemEnvReader
      .getPropertyF(dataDirVarName)
      .map(new File(_))

  private def readDataDirJavaProperty: Either[SqlppError, File] =
    SqlppSystemPropertyReader
      .getPropertyF(dataDirVarName)
      .map(new File(_))


  private def readXdgConfigSubdir: Either[SqlppError, File] = {
    val xdgConfigHome = XdgConfigHome.get

    val configDir: Either[SqlppError, File] = {
      val res = xdgConfigHome
        .flatMap { d =>
          for {
            _ <- CheckFile.checkExists(d)
            _ <- CheckFile.checkIsDirectory(d)
            _ <- CheckFile.checkReadable(d)
          } yield { (true, d)  }
        }

      res.filterOrElse(_._1 == true,
        new DataDirError(s"Directory " +
          s"indicated by ${XdgConfigHome.xdgConfigHomeVarName}" +
          s" is not suitable"))
        .map(_._2)
    }

    configDir
      .map(xdgConfigHome => new File(xdgConfigHome, xdgConfigSubdirName))
  }

  private def readHomeSubdir: Either[SqlppError, File] = {
    UserHome
      .get
      .map(home => new File(home, homeSubdirName))
  }

  private def getPrioritiedPref(cliArg: Option[File]): PrioritizedPref[File] = {
    def getCliArg: () => Either[SqlppError, File] = { () =>
      cliArg match {
        case Some(x) => Right(x)
        case None => Left(new config.PrioritizedPref.EmptyOptionError(
          "No CLI arg passed"))
      }
    }

    val orderedGetters: PrioritizedPref.OrderedGetters[File] = Seq(
      getCliArg,
      () => readDataDirJavaProperty,
      () => readDataDirEnvVariable,
      () => readXdgConfigSubdir,
      () => readHomeSubdir
    )

    new PrioritizedPref[File](orderedGetters)
  }

  object CheckDir {
    private def checkDataDir(d: File): Either[SqlppError, File] = {
      for {
        _ <- CheckFile.checkExists(d)
        _ <- CheckFile.checkIsDirectory(d)
        _ <- CheckFile.checkReadable(d)
        _ <- CheckFile.checkWritable(d)
        _ <- CheckFile.checkExecutable(d)
      } yield { d }
    }

    private def checkOrMakeDir(d: File): Either[SqlppError, File] = {
      for {
        _ <- CheckFile.mkDir(d)
        _ <- CheckFile.setReadable(true)(d)
        _ <- CheckFile.setWritable(true)(d)
        _ <- CheckFile.setExecutable(true)(d)
      } yield { d }
    }

    def apply(d: File): Either[SqlppError, File] = {
      checkOrMakeDir(d)
        .flatMap(checkDataDir)
    }
  }

  def get(cliArg: Option[File]): Either[SqlppError, File] = {
    getPrioritiedPref(cliArg)()
      //check the chosen dir before returning
      .flatMap(CheckDir.apply)
  }
}
