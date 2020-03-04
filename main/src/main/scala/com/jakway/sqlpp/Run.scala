package com.jakway.sqlpp

import java.io.File

import com.jakway.sqlpp.config.CreateProfileDirOption.{CreateDefaultProfileDir, NoCreateProfileDir}
import com.jakway.sqlpp.config.checked
import com.jakway.sqlpp.config.checked.profiledir.CreateProfileDir
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

  //TODO: parse config
  private def parse(args: Array[String]):
    Either[error.SqlppError, checked.Config] = {

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

    def createProfileDir(dest: File): Either[SqlppError, Unit] =
      CreateProfileDir.createProfileDir(config.)

    config.createProfileDirOption match {
      case NoCreateProfileDir => Right({})
      case CreateDefaultProfileDir => {

      }
    }
  }
}