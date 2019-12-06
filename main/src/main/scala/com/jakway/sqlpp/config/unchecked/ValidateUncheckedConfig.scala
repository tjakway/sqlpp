package com.jakway.sqlpp.config.unchecked

import java.io.File

import com.jakway.sqlpp.config.Config
import com.jakway.sqlpp.config.error.{InvalidLoaderTypesError, NoSourcePassedError}
import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType

object ValidateUncheckedConfig {
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
