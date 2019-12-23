package com.jakway.sqlpp

import com.jakway.sqlpp.config.checked
import com.jakway.sqlpp.config.output.OutputTarget
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.TemplateEngine

object Main {
  def main(args: Array[String]): Unit = apply(args)

  def apply(args: Array[String]): Unit = {
    //TODO: parse config
  }

  def apply(checkedConfig: checked.Config): Either[SqlppError, Unit] = {
    for {
      ioMap <- checkedConfig.getIOMap
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