package com.jakway.sqlpp.config.unchecked

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}
import java.nio.charset.{Charset, StandardCharsets}

import com.jakway.sqlpp.config.checked.Config
import com.jakway.sqlpp.config.entries.ParseOutputPattern
import com.jakway.sqlpp.config.error.{ConfigError, InvalidLoaderTypesError, NoSourcePassedError}
import com.jakway.sqlpp.config.unchecked.ValidateUncheckedConfig.Errors.{BadSourceError, OpenSourceError, UnknownCharsetError}
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

  object Errors {
    class ValidateUncheckedConfigError(override val msg: String)
      extends ConfigError(msg)

    class UnknownCharsetError(val encodingName: String,
                              val throwable: Throwable)
      extends ValidateUncheckedConfigError(
        s"Did not recognize charset $encodingName;" +
          "caused error: " + SqlppError.formatThrowable(throwable))

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
      ???
    }

    private def checkEncoding(encodingName: String): Either[SqlppError, String] = {
      TryToEither(new UnknownCharsetError(encodingName, _))(
        Try(Charset.forName(encodingName).displayName()))
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
