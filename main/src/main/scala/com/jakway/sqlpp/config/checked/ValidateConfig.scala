package com.jakway.sqlpp.config.checked

import java.io.File

import com.jakway.sqlpp.config.error.OutputTargetErrors
import com.jakway.sqlpp.config.output.OutputTarget
import com.jakway.sqlpp.error.{CheckFile, SqlppError}

object ValidateConfig {
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

  /*
  TODO
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
   */
}
