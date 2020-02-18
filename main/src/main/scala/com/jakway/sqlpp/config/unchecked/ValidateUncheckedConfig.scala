package com.jakway.sqlpp.config.unchecked

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}
import java.nio.charset.Charset

import com.jakway.sqlpp.config.{CreateProfileDirOption, Defaults, checked}
import com.jakway.sqlpp.config.entries.ParseOutputPattern
import com.jakway.sqlpp.config.error.{ConfigError, InvalidLoaderTypesError, NoSourcePassedError}
import com.jakway.sqlpp.config.output.{OutputPattern, OutputTarget}
import com.jakway.sqlpp.config.unchecked.ValidateUncheckedConfig.Errors._
import com.jakway.sqlpp.error.{CheckFile, CheckString, SqlppError}
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.ExtraTemplateOptions
import com.jakway.sqlpp.template.backend.MultiAttemptBackend
import com.jakway.sqlpp.util.{FileUtil, TryToEither}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

//TODO: warn if extraJars is nonempty but the list
// of loader types doesn't contain JarLoader
object ValidateUncheckedConfig {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def check(uncheckedConfig: UncheckedConfig): Either[SqlppError, checked.Config] = {
    Check(uncheckedConfig)
  }

  def apply: UncheckedConfig => Either[SqlppError, checked.Config] = check

  object Errors {
    class ValidateUncheckedConfigError(override val msg: String)
      extends ConfigError(msg)

    class ProfileDirError(override val msg: String)
      extends ValidateUncheckedConfigError(msg)

    class UnknownCharsetError(val encodingName: String,
                              val throwable: Throwable)
      extends ValidateUncheckedConfigError(
        s"Did not recognize charset $encodingName;" +
          "caused error: " + SqlppError.formatThrowable(throwable))

    class TargetBackendsError(override val msg: String)
      extends ValidateUncheckedConfigError(msg)

    object NoTargetBackendsError
      extends TargetBackendsError(
        "Must have at least 1 target backend")

    class OutputPatternError(override val msg: String)
      extends ValidateUncheckedConfigError(msg)

    class SourceError(override val msg: String)
      extends ValidateUncheckedConfigError(msg)

    class OpenSourceError(override val msg: String)
      extends SourceError(msg) {

      def this(source: String,
               throwable: Throwable) {
        this(s"Error opening source $source: " +
            SqlppError.formatThrowable(throwable))
      }
    }


    class BadSourceError(override val msg: String)
      extends SourceError(msg)

    object NoSourcePassedError
      extends SourceError("Need input source.")

    class ExtraTemplateOptionsError(override val msg: String)
      extends ValidateUncheckedConfigError(msg)
  }

  private object ParseSource {
    private def parseCheckedSource:
      String => Either[SqlppError, InputStream] = { fileName =>

      val f = new File(fileName)

      def openStream(g: File): Either[SqlppError, InputStream] = {
        TryToEither(new OpenSourceError(g.toString, _)) {
          Try(new BufferedInputStream(new FileInputStream(g)))
        }
      }

      for {
        _ <- CheckFile.checkExists(f)
        _ <- CheckFile.checkIsFile(f)
        _ <- CheckFile.checkReadable(f)
        res <- openStream(f)
      } yield {
        res
      }
    }

    def parseSource(source: Option[String],
                    noSourceImpliesStdin: Boolean):
    Either[SqlppError, InputStream] = {

      def useStdin() = {
        logger.debug("Reading from stdin")
        Right(System.in)
      }

      source.map(_.trim) match {
        case Some(x) if CheckString.isEmpty(x) =>
          Left(new BadSourceError("Passed source string is empty"))

        case Some(x) => {
          if(x == ParseOutputPattern.stdoutSpecialChar.toString) {
            useStdin()
          } else {
            parseCheckedSource(x)
          }
        }

        case None if noSourceImpliesStdin => {
          useStdin()
        }

        case None => Left(NoSourcePassedError)
      }

    }
  }

  private object Check {
    def apply(uncheckedConfig: UncheckedConfig): Either[SqlppError, checked.Config] = {
      //TODO

      val requireFormatSymbol: Boolean =
        uncheckedConfig.targetBackends.size > 1
      val uncheckedEncoding = getUncheckedEncoding(uncheckedConfig)

      for {
        checkedEncoding <- checkEncoding(uncheckedEncoding)

        createProfileDirOption <- checkCreateProfileDir(
          uncheckedConfig.noCreateProfileDir,
          uncheckedConfig.createProfileDir)

        outputPattern <- ParseOutputPattern(
          checkedEncoding,
          requireFormatSymbol,
          uncheckedConfig.outputTemplate)

        loaderTypes <- CheckLoaderTypes(uncheckedConfig.resourceLoaderTypes)

        source <- ParseSource.parseSource(
          uncheckedConfig.source,
          uncheckedConfig.noSourceImpliesStdin)

        extraTemplateOptions <- CheckExtraTemplateOptions.apply(uncheckedConfig)
      } yield {
        checked.Config(
          source,
          null, //TODO
          checkedEncoding,
          checkedEncoding,
          loaderTypes,
          extraTemplateOptions,
          Defaults.TemplateStringInfo.default,
          createProfileDirOption)
      }
    }

    /**
     * TODO: may have to change this if it often returns too many names
     * that overlap with other backends
     * @param identifier
     * @return
     */
    private def backendIdentifierToNames(identifier: String): Set[String] = {
      def nameWithoutExtension = {
        Try {
          val f = new File(identifier)
          Set(FileUtil.nameWithoutExtension(f))
        }.getOrElse(Set.empty)
      }

      def nameWithoutSeparator(separator: String): Set[String] = {
        if(identifier.contains(separator)) {
          Set(identifier.substring(identifier.lastIndexOf(separator)))
        } else {
          Set.empty
        }
      }

      (Set(identifier) ++
        nameWithoutExtension ++
        nameWithoutSeparator(File.pathSeparator) ++
        nameWithoutSeparator(File.separator))
        .map(_.trim)
        .filter(_.isEmpty)
    }

    private def outputPatternToOutputTargets(outputPattern: OutputPattern,
                                             targetBackends: Seq[String]):
      Seq[OutputTarget] = {

      //convert the backend strings to backend objects
      val backends = targetBackends.map(backendIdentifier =>
        new MultiAttemptBackend(
          backendIdentifierToNames(backendIdentifier),
          Seq(new File(backendIdentifier)),
          Seq(backendIdentifier)))

      outputPattern.permutateBackendDests(backends)
    }

    private def getUncheckedEncoding(uncheckedConfig: UncheckedConfig): String = {
      uncheckedConfig
        .inputEncoding
        .getOrElse(Defaults.defaultEncoding.displayName())
    }

    private def checkCreateProfileDir(noCreateProfileDir: Boolean,
                                      createProfileDir: Option[String]):
      Either[SqlppError, CreateProfileDirOption] = {
      if(noCreateProfileDir && createProfileDir.isDefined) {
        Left(new ProfileDirError(
          UncheckedConfig.OptionNames.noCreateProfileDir +
            " is incompatible with " +
            UncheckedConfig.OptionNames.createProfileDir))
      } else {

        CreateProfileDirOption.parse(
          createProfileDir,
          UncheckedConfig.OptionNames.createProfileDir)
      }
    }


    private def checkEncoding(encodingName: String): Either[SqlppError, String] = {
      TryToEither(new UnknownCharsetError(encodingName, _))(
        Try(Charset.forName(encodingName).displayName()))
    }

    private object ParseOutputPattern {
      //re-importing to refer to the other object as
      //entries.ParseOutputPattern to avoid confusion
      import com.jakway.sqlpp.config.entries

      private val logger: Logger = LoggerFactory.getLogger(getClass)
      /**
       * We require a format symbol if the number of target backends
       * is > 1 because without it the format string will not
       * resolve to enough unique locations
       * @param encoding
       * @param numTargetBackends
       * @return
       */
      def apply(encoding: String,
                numTargetBackends: Int,
                pattern: String):
        Either[SqlppError, OutputPattern] = {

        if(numTargetBackends <= 0) {
          Left(NoTargetBackendsError)
        } else {
          apply(encoding, numTargetBackends > 1, pattern)
        }
      }

      def apply(encoding: String,
                requireFormatSymbol: Boolean,
                pattern: String):
        Either[SqlppError, OutputPattern] = {
        new entries.ParseOutputPattern(encoding)(pattern, requireFormatSymbol)
      }

      def apply(encoding: String,
                requireFormatSymbol: Boolean,
                pattern: Option[String]):
        Either[SqlppError, OutputPattern] = {

        pattern match {
          case Some(x) => {
            logger.debug("Parsing output pattern %s", x)
            apply(encoding, requireFormatSymbol, x)
          }
          case None => {
            logger.debug("Empty output pattern, assuming stdout")
            apply(encoding, requireFormatSymbol,
              entries.ParseOutputPattern.stdoutSpecialChar.toString)
          }
        }
      }
    }

    private object CheckLoaderTypes {
      private def checkLoaderTypes(loaderTypes: Set[LoaderType]): Either[SqlppError, Set[LoaderType]] = {
        if(loaderTypes.isEmpty) {
          Left(new InvalidLoaderTypesError("Need at least 1 resource loader type"))
        } else {
          Right(loaderTypes)
        }
      }

      def apply(loaderTypeNames: Set[String]):
        Either[SqlppError, Set[LoaderType]] = {

        val empty: Either[SqlppError, Set[LoaderType]] = Right(Set.empty)
        loaderTypeNames.foldLeft(empty) {
          case (eAcc, thisLoaderTypeName) => eAcc.flatMap { acc =>
            LoaderType
              .read(thisLoaderTypeName)
              .map(res => acc + res)
          }
        }
        .flatMap(checkLoaderTypes)
      }
    }

    private object CheckExtraTemplateOptions {
      def apply: UncheckedConfig => Either[SqlppError, ExtraTemplateOptions] =
        checkExtraTemplateOptions

      private def err(throwable: Throwable): SqlppError =
        new ExtraTemplateOptionsError(
          "Error in additional template options: " +
        SqlppError.formatThrowableCause(throwable))

      private def getCurrentWorkingDirectory: Either[SqlppError, File] = {
        TryToEither(err)(Try(new File(".")))
      }

      private def checkExtraTemplateOptions(
        uncheckedConfig: UncheckedConfig):
        Either[SqlppError, ExtraTemplateOptions] = {

        for {
          extraDirs <- checkExtraDirs(uncheckedConfig.extraDirs)
          extraJars <- checkExtraJars(uncheckedConfig.extraJars)
          cwd <- getCurrentWorkingDirectory
        } yield {
          //add the current working directory to the list if it doesn't contain it already
          //TODO: add a config option to disable this behavior
          val finalDirs =
            if(extraDirs.contains(cwd)) {
              extraDirs
            } else {
              extraDirs :+ cwd
            }

          ExtraTemplateOptions(
            //TODO: make this a config option
            Defaults.extraTemplateOptions.stringRepositoryName,
            finalDirs,
            extraJars
          )
        }
      }

      private def checkExtraDirs(dirs: Seq[String]):
        Either[SqlppError, Seq[File]] = {

        val zero: Either[SqlppError, Seq[File]] = Right(Seq.empty)
        dirs.foldLeft(zero) {
          case (eAcc, thisDir) => eAcc.flatMap { acc =>
            checkDir(thisDir).map(res => acc :+ res)
          }
        }
      }

      //TODO: consolidate with checkExtraDirs to eliminate redundancy
      private def checkExtraJars(jars: Seq[String]):
        Either[SqlppError, Seq[String]] = {

        val zero: Either[SqlppError, Seq[String]] = Right(Seq.empty)
        jars.foldLeft(zero) {
          case (eAcc, thisJar) => eAcc.flatMap { acc =>
            checkJar(thisJar).map(res => acc :+ res)
          }
        }
      }

      private def checkDir(d: String): Either[SqlppError, File] = {
        val checks = Seq(
          CheckFile.checkExists,
          CheckFile.checkIsDirectory,
          CheckFile.checkReadable,
          CheckFile.checkExecutable)

        runChecks(checks, d)
      }

      private def runChecks(checks: Seq[CheckFile.FileCheckF],
                            name: String): Either[SqlppError, File] = {

        TryToEither(err)(Try(new File(name)))
          .flatMap { dir =>
            CheckFile.composeAll(checks)(dir)
              .map(_ => dir)
          }
      }

      private def checkJar(j: String): Either[SqlppError, String] = {
        val checks = Seq(
          CheckFile.checkExists,
          CheckFile.checkIsFile,
          CheckFile.checkReadable)

        runChecks(checks, j).map(f => f.getAbsolutePath)
      }
    }
  }
}
