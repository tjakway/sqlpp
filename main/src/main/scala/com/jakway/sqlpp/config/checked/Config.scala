package com.jakway.sqlpp.config.checked

import java.io.File
import java.nio.charset.{Charset, StandardCharsets}

import com.jakway.sqlpp.config.OutputTarget
import com.jakway.sqlpp.template.GeneralVelocityOptions
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.{ExtraTemplateOptions, PropertyMap}

case class Config(source: File,
                  outputTargets: Seq[OutputTarget],
                  allowOverwrite: Boolean,
                  inputEncoding: String,
                  resourceLoaderTypes: Set[LoaderType],
                  extraTemplateOptions: ExtraTemplateOptions) {

  val additionalVelocityProperties: PropertyMap =
    GeneralVelocityOptions(inputEncoding)
}

object Config {
  object Defaults {
    val defaultEncoding: Charset = StandardCharsets.UTF_8
    val allowOverwrite: Boolean = false
    val extraTemplateOptions: ExtraTemplateOptions = ExtraTemplateOptions(Seq(), Seq())
  }
}
