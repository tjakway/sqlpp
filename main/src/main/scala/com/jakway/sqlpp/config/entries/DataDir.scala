package com.jakway.sqlpp.config.entries

import java.io.File

import com.jakway.sqlpp.config
import com.jakway.sqlpp.config.PrioritizedPref
import com.jakway.sqlpp.config.env.{SqlppSystemEnvReader, SqlppSystemPropertyReader, UserHome, XdgConfigHome}
import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import org.slf4j.{Logger, LoggerFactory}

object DataDir {
  val dataDirVarName: String = "SQLPP_DIR"
  val xdgConfigSubdirName: String = "sqlpp"
  val homeSubdirName: String = ".sqlpp"

  private val logger: Logger = LoggerFactory.getLogger(getClass)

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

  def get(cliArg: Option[File]): Either[SqlppError, File] = {
    getPrioritiedPref(cliArg)()
  }

}
