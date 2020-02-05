package com.jakway.sqlpp.config.unchecked

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}
import java.nio.charset.Charset

import com.jakway.sqlpp.config.Defaults
import com.jakway.sqlpp.config.checked.Config
import com.jakway.sqlpp.config.entries.ParseOutputPattern
import com.jakway.sqlpp.config.error.{ConfigError, InvalidLoaderTypesError, NoSourcePassedError}
import com.jakway.sqlpp.config.output.OutputPattern
import com.jakway.sqlpp.config.unchecked.ValidateUncheckedConfig.Errors._
import com.jakway.sqlpp.error.{CheckFile, CheckString, SqlppError}
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.util.TryToEither
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

object ValidateUncheckedConfig {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def check(uncheckedConfig: UncheckedConfig): Either[SqlppError, Config] = {
    Check(uncheckedConfig)
  }

  def apply: UncheckedConfig => Either[SqlppError, Config] = check

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
    def apply(uncheckedConfig: UncheckedConfig): Either[SqlppError, Config] = {
      //TODO

      val requireFormatSymbol: Boolean =
        uncheckedConfig.targetBackends.size > 1
      val uncheckedEncoding = getUncheckedEncoding(uncheckedConfig)

      for {
        checkedEncoding <- checkEncoding(uncheckedEncoding)

        createProfileDirOption <- checkCreateProfileDir(
          uncheckedConfig.noCreateProfileDir,
          uncheckedConfig.createProfileDir)

        outputPattern <- parseOutputPattern(
          checkedEncoding,
          requireFormatSymbol,
          uncheckedConfig.outputTemplate)
      } yield {
        ???
      }
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

    private def checkLoaderTypes(loaderTypes: Set[LoaderType]): Either[SqlppError, Set[LoaderType]] = {
      if(loaderTypes.isEmpty) {
        Left(new InvalidLoaderTypesError("Need at least 1 resource loader type"))
      } else {
        Right(loaderTypes)
      }
    }
  }
}
