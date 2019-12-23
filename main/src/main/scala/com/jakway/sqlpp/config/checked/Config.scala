package com.jakway.sqlpp.config.checked

import java.io.InputStream

import com.jakway.sqlpp.config.output.OutputTarget
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.{GeneralVelocityOptions, TemplateEngine}
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.{ExtraTemplateOptions, PropertyMap}

case class Config(source: InputStream,
                  outputTargets: Seq[OutputTarget],
                  inputEncoding: String,
                  outputEncoding: String,
                  resourceLoaderTypes: Set[LoaderType],
                  extraTemplateOptions: ExtraTemplateOptions) {

  val additionalVelocityProperties: PropertyMap =
    GeneralVelocityOptions(inputEncoding)

  def getTemplateEngine: Either[SqlppError, TemplateEngine] =
    Function.uncurried(TemplateEngine.apply)(
      inputEncoding,
      resourceLoaderTypes,
      extraTemplateOptions,
      additionalVelocityProperties)

  def getIOMap: Either[SqlppError, TemplateEngine.IOMap] =
    OutputTarget.toIOMap(outputTargets, outputEncoding)
}
