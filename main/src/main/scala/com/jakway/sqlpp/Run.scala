package com.jakway.sqlpp

import com.jakway.sqlpp.config.checked
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

    fmt.format("Error, message follows\n%s\n", sqlppError.print)
    fmt.toString
  }

  //TODO: parse config
  private def parse(args: Array[String]):
    Either[error.SqlppError, checked.Config] = ???

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
}