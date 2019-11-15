package com.jakway.sqlpp

import java.io.File

import com.jakway.sqlpp.error.SqlppError

case class Config(source: File,
                  outputTargets: Seq[OutputTarget],
                  allowOverwrite: Boolean)

object Config {
  def default(source: File): Config =
    Config(source, Seq(), false)

  def check(config: Config): Either[SqlppError, Config] = {

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
