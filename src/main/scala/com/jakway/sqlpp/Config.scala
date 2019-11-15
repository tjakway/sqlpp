package com.jakway.sqlpp

import java.io.File

import com.jakway.sqlpp.error.{CheckFile, SqlppError}

case class Config(source: File,
                  outputTargets: Seq[OutputTarget],
                  allowOverwrite: Boolean)

object Config {
  class ConfigError(val causes: Seq[SqlppError])
    extends SqlppError(SqlppError.formatErrors(causes))

  class OutputTargetErrors(override val causes: Seq[SqlppError])
    extends ConfigError(causes)

  def default(source: File): Config =
    Config(source, Seq(), false)

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
    } yield {
      config
    }
  }
}

case class OutputTarget(backend: Backend,
                        dest: File)

case class Backend(names: Set[String]) {
  def matches(x: String): Boolean =
    names.exists(Backend.areEqual(_, x))
}

object Backend {
  def areEqual(left: String, right: String): Boolean = {
    def normalizeString(str: String): String = str.trim.toLowerCase()

    normalizeString(left) == normalizeString(right)
  }
}
