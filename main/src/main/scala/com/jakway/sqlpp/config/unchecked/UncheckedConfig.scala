package com.jakway.sqlpp.config.unchecked

import java.io.File

import com.jakway.sqlpp.config.{Constants, Defaults, VerbosityLevel}
import com.jakway.sqlpp.config.Defaults.{UncheckedConfig => UncheckedConfigDefaults}
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType

case class UncheckedConfig(verbosityLevel: VerbosityLevel =
                             Defaults.VerbosityLevel.default,
                           source: Option[File] = None,
                           outputTemplate: Option[String] = None,
                           inputEncoding: Option[String] = None,
                           targetBackends: Seq[String] = Seq(),
                           addBackendLocations: Set[String] = Set(),
                           resourceLoaderTypes: Set[String] = Set(),
                           noCreateProfileDir: Boolean =
                             UncheckedConfigDefaults.defaultNoCreateProfileDir,
                           createProfileDir: Option[String],
                           allowOverwrite: Boolean = Defaults.allowOverwrite)

object UncheckedConfig {

  private def parser(defaultConfigDir: String) = {
    import scopt.OParser
    val builder = OParser.builder[UncheckedConfig]
    import builder._

    OParser.sequence(
      programName(Constants.programName),
      head(Constants.programName, Constants.version),

      opt[File]('s', "source")
        .required()
        .action((x, c) => c.copy(source = Some(x))),

      opt[String]('t', "output-template")
        .required()
        .text("Template to process the source through." +
          " Can be a path or resource identifier if using" +
          " the jar or classpath resource loaders.")
        .valueName("<apache velocity template identifier>")
        .action((x, c) => c.copy(outputTemplate = Some(x))),

      opt[String]('l', "add-backend-location")
        .text("Paths to extra build targets")
        .action((x, c) =>
          c.copy(addBackendLocations = c.addBackendLocations + x))
        .unbounded(),

      opt[Seq[String]]('b', "target-backends")
        .text("Add a comma-separated list of " +
          "backends to the set of target backends.")
        .action((x, c) =>
          c.copy(targetBackends = c.targetBackends ++ x))
        .unbounded(),

      opt[String]("target-backend")
        .text("Add a single backend to the set of target backends.")
        .action((x, c) =>
          c.copy(targetBackends = c.targetBackends :+ x)),

      opt[String]('e', "input-encoding")
        .action((x, c) => c.copy(inputEncoding = Some(x)))
        .text(s"Input and output encoding," +
          s" default: ${Defaults.defaultEncoding}")
        .valueName(s"<encoding>"),

      opt[Boolean]("allow-overwrite")
        .action((x, c) => c.copy(allowOverwrite = x)),

      opt[Seq[String]]("resource-loader-types")
        .action((x, c) => c.copy(resourceLoaderTypes =
          c.resourceLoaderTypes ++ x.toSet)),

      opt[Unit]("no-create-profile-dir")
        .action((_, c) => c.copy(noCreateProfileDir = true)),

      opt[String]("create-profile-dir")
        .action((x, c) => c.copy(createProfileDir = Some(x)))
        .text(s"default location: $defaultConfigDir"),

      opt[Unit]('d', "debug")
        .action((_, c) => c.copy(verbosityLevel = VerbosityLevel.Verbose))
        .text("Enable debug features like stack traces.")
    )
  }
}