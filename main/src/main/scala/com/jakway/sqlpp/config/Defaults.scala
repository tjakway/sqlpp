package com.jakway.sqlpp.config

import java.nio.charset.{Charset, StandardCharsets}

import com.jakway.sqlpp.template.TemplateEngine.ExtraTemplateOptions

object Defaults {
  val defaultEncoding: Charset = StandardCharsets.UTF_8
  val allowOverwrite: Boolean = false
  val extraTemplateOptions: ExtraTemplateOptions = ExtraTemplateOptions(Seq(), Seq())

  object DataDir {
    val dataDirVarName: String = "SQLPP_DIR"
    val xdgConfigSubdirName: String = "sqlpp"
    val homeSubdirName: String = ".sqlpp"
  }

  object TemplateStringInfo {
    val defaultTemplateSourceKey: String = "__SQLPP_TEMPLATE_SOURCE_KEY"
    val defaultStringResourceRepositoryName: String = "__SQLPP_STRING_RESOURCE_REPOSITORY"
  }
}
