package com.jakway.sqlpp.config.unchecked

import java.io.File

import com.jakway.sqlpp.config.Defaults.{UncheckedConfig => UncheckedConfigDefaults}
import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.config.{Constants, Defaults, VerbosityLevel}
import com.jakway.sqlpp.error.SqlppError
import org.slf4j.{Logger, LoggerFactory}
import scopt.{DefaultOParserSetup, OParserSetup}

//TODO: add StringRepositoryName parameter (probably
// will only be used for debugging purposes)
case class UncheckedConfig(verbosityLevel: VerbosityLevel =
                             Defaults.VerbosityLevel.default,
                           source: Option[String] = None,
                           outputTemplate: Option[String] = None,
                           inputEncoding: Option[String] = None,
                           targetBackends: Seq[String] = Seq(),
                           addBackendLocations: Set[String] = Set(),
                           resourceLoaderTypes: Set[String] = Set(),
                           extraDirs: Seq[String] = Seq(),
                           extraJars: Seq[String] = Seq(),
                           noCreateProfileDir: Boolean =
                             UncheckedConfigDefaults.defaultNoCreateProfileDir,
                           createProfileDir: Option[String] = None,
                           deleteProfileDirOnCreationFailure: Boolean =
                             Defaults.DataDir.deleteOnFailure,
                           allowOverwrite: Boolean = Defaults.allowOverwrite,
                           noSourceImpliesStdin: Boolean =
                             Defaults.noSourceImpliesStdin) {

  protected def addDir(d: String): UncheckedConfig = {
    copy(extraDirs = this.extraDirs :+ d)
  }

  protected def addJar(j: String): UncheckedConfig = {
    copy(extraJars = this.extraJars :+ j)
  }
}

object UncheckedConfig {
  import scopt.OParser

  val logger: Logger = LoggerFactory.getLogger(getClass)

  class CLIParsingFailedError(override val msg: String)
    extends ConfigError(msg)

  case class CLIParsingFailedWithMessage(override val msg: String)
    extends CLIParsingFailedError(msg)

  case object CLIParsingFailedWithoutMessage
    extends CLIParsingFailedError(getClass.getName)

  sealed abstract class CreateProfileDirOption

  object CreateProfileDirOption {
    case object NoCreateProfileDir extends CreateProfileDirOption
    class CreateProfileDir extends CreateProfileDirOption
    case object CreateDefaultProfileDir extends CreateProfileDirOption
    case class CreateUserPassedProfileDir(toCreate: File)
      extends CreateProfileDirOption
  }

  object OptionNames {
    private def optionPrefix: String = "--"
    private def prefix(name: String): String =
      optionPrefix + name

    val noCreateProfileDir: String = prefix("no-create-profile-dir")
    val createProfileDir: String = prefix("create-profile-dir")

    val addDir: String = prefix("add-dir")
    val addJar: String = prefix("add-jar")
  }

  @Deprecated
  def parseOrExit(defaultConfigDir: String)
                 (args: Array[String]): UncheckedConfig = {
    val p = parser
    OParser.parse(p, args, UncheckedConfig()).get
  }

  def parse(args: Array[String]):
    Either[SqlppError, (Option[String], UncheckedConfig)] = {

    val setup = new ParserSetup()
    val res = OParser.parse(
      parser,
      args,
      UncheckedConfig(),
      setup)

    res match {
        //parsing successful, check for warnings
      case Some(parsedConfig) => {
        setup.getErrors match {
          case Some(msg) => {
            logger.warn("Parser returned config with" +
              " additional messages: %s", msg)
          }
          case _ => {}
        }

        Right((setup.getWarnings, parsedConfig))
      }

        //parsing failed, return error messages
      case None => {
        setup.getErrors match {
          case Some(msg) => Left(CLIParsingFailedWithMessage(msg))
          case None => Left(CLIParsingFailedWithoutMessage)
        }
      }
    }
  }

  private def parser = {
    val builder = OParser.builder[UncheckedConfig]
    import builder._

    OParser.sequence(
      programName(Constants.programName),
      head(Constants.programName, Constants.version),

      opt[Option[String]]('s', "source")
        .action((x, c) => c.copy(source = x)),

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
        .text(s"Default location is XDG-compliant"),

      opt[Boolean]("delete-profile-dir-on-creation-failure")
        .action((x, c) => c.copy(deleteProfileDirOnCreationFailure = x))
        .text("Clean up our attempt to create a profile dir if we fail" +
          s" (default: ${Defaults.DataDir.deleteOnFailure}"),

      opt[String]('d', "add-dir")
        .action((x, c) => c.addDir(x))
        .text("Add directories to the list" +
          " Apache Velocity will search for templates " +
          "(default: the current working directory).")
        .unbounded(),

      opt[String]('j', "add-jar")
        .action((x, c) => c.addDir(x))
        .text("Add jars to the list" +
          " Apache Velocity will search for templates" +
          "(default: none).")
        .unbounded(),

      opt[Seq[String]]("set-dir-list")
        .action((x, c) => c.copy(extraDirs = x))
        .text("Set the list of directories Apache Velocity will" +
          s" search for templates (replaces ${OptionNames.addDir})."),

      opt[Seq[String]]("set-jar-list")
        .action((x, c) => c.copy(extraDirs = x))
        .text("Set the list of jars Apache Velocity will" +
          s" search for templates (replaces ${OptionNames.addJar})."),

      opt[Unit]('v', "verbose")
        .action((_, c) => c.copy(verbosityLevel = VerbosityLevel.Verbose))
        .text("Enable debug features like stack traces.")
    )
  }
}