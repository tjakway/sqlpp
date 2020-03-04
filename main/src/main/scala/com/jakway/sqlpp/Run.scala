package com.jakway.sqlpp

import java.io.File

import com.jakway.sqlpp.config.CreateProfileDirOption.{CreateDefaultProfileDir, CreateUserPassedProfileDir, NoCreateProfileDir}
import com.jakway.sqlpp.config.checked
import com.jakway.sqlpp.config.checked.profiledir.{CreateProfileDir, GetDefaultProfileDirLocation}
import com.jakway.sqlpp.config.unchecked.{UncheckedConfig, ValidateUncheckedConfig}
import com.jakway.sqlpp.config.unchecked.UncheckedConfig.CLIParsingFailedWithMessage
import com.jakway.sqlpp.error.SqlppError

object Run {
  case class Result(exitCode: Int, errorMessage: Option[String]) {
    def success: Boolean = exitCode == Result.successCode
  }

  object Result {
    val successCode: Int = 0
  }

  def printErrorMessage(sqlppError: SqlppError): String = {
    val fmt = new java.util.Formatter(new StringBuffer())

    def print: String = {
      sqlppError match {
          //handle option parsing errors to be more traditional
          //(don't print the class containing the error)
        case CLIParsingFailedWithMessage(msg) => msg
        case _ => sqlppError.print
      }
    }

    fmt.format("Error, message follows\n%s\n", print)
    fmt.toString
  }


  /**
   * *******************************
   * *******Entry Point*************
   * *******************************
   */
  def main(args: Array[String]): Unit = {
    val res = apply(args)
    res.errorMessage.foreach(System.err.println)
    System.exit(res.exitCode)
  }
  /**
   * *******************************
   * *******************************
   * *******************************
   */


  def parse(args: Array[String]):
    Either[error.SqlppError, checked.Config] = {
    UncheckedConfig
      .parse(args)
      .flatMap(ValidateUncheckedConfig.apply)
  }

  def printParseResult(args: Array[String]): Unit = {
    println(parse(args))
  }

  def apply(args: Array[String]): Result = {
    parse(args).flatMap(apply) match {
      case Right(_) => Result(Result.successCode, None)
      case Left(error) =>
        Result(1, Some(printErrorMessage(error)))
    }
  }

  def apply(checkedConfig: checked.Config): Either[SqlppError, Unit] = {
    val ioMap = checkedConfig.ioMap
    for {
      _ <- createProfileDir(checkedConfig)
      templateEngine <- checkedConfig.getTemplateEngine
      template <- templateEngine
        .loadTemplateFromInputStream(
          checkedConfig.source)(
          checkedConfig.templateStringInfo.templateSourceKey)(
          checkedConfig.templateStringInfo.stringResourceRepositoryName)(
          checkedConfig.inputEncoding)

      _ <- templateEngine.multiApplyWriters(template, ioMap)
    } yield {{}}
  }

  private def createProfileDir(config: checked.Config):
    Either[SqlppError, Unit] = {

    //wrap args
    def createProfileDir(dest: File): Either[SqlppError, Unit] =
      CreateProfileDir.createProfileDir(
        config.backends,
        dest,
        config.outputEncoding,
        config.deleteProfileDirOnCreationFailure)

    config.createProfileDirOption match {
      case NoCreateProfileDir => Right({})
      case CreateDefaultProfileDir => {
        GetDefaultProfileDirLocation
          .apply()
          .map(createProfileDir)
      }

      case CreateUserPassedProfileDir(dest) =>
        createProfileDir(dest)
    }
  }
}