package com.jakway.sqlpp.config.checked

import java.io.InputStream

import com.jakway.sqlpp.config.output.OutputTarget
import com.jakway.sqlpp.template.GeneralVelocityOptions
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.{ExtraTemplateOptions, PropertyMap}

case class Config(source: InputStream,
                  outputTargets: Seq[OutputTarget],
                  inputEncoding: String,
                  resourceLoaderTypes: Set[LoaderType],
                  extraTemplateOptions: ExtraTemplateOptions) {

  val additionalVelocityProperties: PropertyMap =
    GeneralVelocityOptions(inputEncoding)
}
