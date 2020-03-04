package com.jakway.sqlpp.config.checked

import java.io.InputStream

import com.jakway.sqlpp.config.{CreateProfileDirOption, Defaults}
import com.jakway.sqlpp.config.output.OutputTarget
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.{GeneralVelocityOptions, TemplateEngine}
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.{ExtraTemplateOptions, PropertyMap}
import com.jakway.sqlpp.template.backend.Backend

/**
 * TODO: consolidate stringResourceRepositoryName with ExtraTemplateOptions
 * @param templateSourceKey
 * @param stringResourceRepositoryName
 */
case class TemplateStringInfo(templateSourceKey: String =
                                Defaults.TemplateStringInfo
                                  .defaultTemplateSourceKey,
                              stringResourceRepositoryName: String =
                                Defaults.TemplateStringInfo
                                  .defaultStringResourceRepositoryName)

case class Config(source: InputStream,
                  ioMap: TemplateEngine.IOMap,
                  inputEncoding: String,
                  outputEncoding: String,
                  resourceLoaderTypes: Set[LoaderType],
                  extraTemplateOptions: ExtraTemplateOptions,
                  templateStringInfo: TemplateStringInfo,
                  createProfileDirOption: CreateProfileDirOption,
                  backends: Set[Backend]) {

  val additionalVelocityProperties: PropertyMap =
    GeneralVelocityOptions(inputEncoding)

  def getTemplateEngine: Either[SqlppError, TemplateEngine] =
    Function.uncurried(TemplateEngine.apply)(
      inputEncoding,
      resourceLoaderTypes,
      extraTemplateOptions,
      additionalVelocityProperties)
}
