package com.jakway.sqlpp.config

import java.io.File
import java.nio.charset.{Charset, StandardCharsets}

import com.jakway.sqlpp.Backend
import com.jakway.sqlpp.config.Config.Defaults
import com.jakway.sqlpp.config.error.{InvalidLoaderTypesError, NoSourcePassedError, OutputTargetErrors}
import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import com.jakway.sqlpp.template.GeneralVelocityOptions
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.{ExtraTemplateOptions, PropertyMap}

case class UncheckedConfig(source: Option[File] = None,
                           outputTemplate: Option[String] = None,
                           inputEncoding: Option[String] = None,
                           additionalBuildTargets: Seq[String] = Seq(),
                           resourceLoaderTypes: Set[LoaderType] = Set(),
                           allowOverwrite: Boolean = Defaults.allowOverwrite)

object UncheckedConfig {
  def check(uncheckedConfig: UncheckedConfig): Either[SqlppError, Config] = {
    Check(uncheckedConfig)
  }

  private object Check {
    def apply(uncheckedConfig: UncheckedConfig): Either[SqlppError, Config] = {
      //TODO
      ???
    }

    private def checkLoaderTypes(loaderTypes: Set[LoaderType]): Either[SqlppError, Set[LoaderType]] = {
      if(loaderTypes.isEmpty) {
        Left(new InvalidLoaderTypesError("Need at least 1 resource loader type"))
      } else {
        Right(loaderTypes)
      }
    }

    private def checkSource(source: Option[File]): Either[SqlppError, File] = {
      source match {
        case None => Left(NoSourcePassedError)
        case Some(f) => {
          for {
            _ <- CheckFile.checkExists(f)
            _ <- CheckFile.checkIsFile(f)
            _ <- CheckFile.checkReadable(f)
          } yield {f}
        }
      }
    }
  }

}

object ConfigErrors {

}


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




  private def checkSource(source: File): Either[SqlppError, File] = {
    for {
      _ <- CheckFile.checkIsFile(source)
      _ <- CheckFile.checkReadable(source)
    } yield { source }
  }

  private def checkOutputTarget(outputTarget: OutputTarget): Either[SqlppError, OutputTarget] = {
    //TODO
    ???
  }

  private def checkOutputTargets(targets: Seq[OutputTarget]): Either[SqlppError, Seq[OutputTarget]] = {
    val empty: Either[Seq[SqlppError], Seq[OutputTarget]] = Right(Seq())

    val res = targets.foldLeft(empty) {
      case (eAcc, thisTarget) => {
        (eAcc, checkOutputTarget(thisTarget)) match {
          case (Right(acc), Right(success)) => Right(acc :+ success)
          case (Right(_), Left(failure)) => Left(Seq(failure))
          case (Left(acc), Right(_)) => Left(acc)
          case (Left(acc), Left(failure)) => Left(acc :+ failure)
        }
      }
    }

    res match {
      case Right(xs) => Right(xs)
      case Left(errors) => Left(new OutputTargetErrors(errors))
    }
  }

  def check(config: Config): Either[SqlppError, Config] = {
    //TODO: more user-friendly error printing

    for {
      checkedSource <- checkSource(config.source)
      checkedOutputTargets <- checkOutputTargets(config.outputTargets)
    } yield {
      config
        .copy(source = checkedSource)
        .copy(outputTargets = checkedOutputTargets)
    }
  }
}

case class OutputTarget(backend: Backend,
                        dest: File)
