package com.jakway.sqlpp.config.unchecked

import java.io.File

import com.jakway.sqlpp.config.checked.Config.Defaults
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType

case class UncheckedConfig(source: Option[File] = None,
                           outputTemplate: Option[String] = None,
                           inputEncoding: Option[String] = None,
                           additionalBuildTargets: Seq[String] = Seq(),
                           resourceLoaderTypes: Set[LoaderType] = Set(),
                           allowOverwrite: Boolean = Defaults.allowOverwrite)
