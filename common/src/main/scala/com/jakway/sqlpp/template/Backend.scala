package com.jakway.sqlpp.template

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.Backend.NamelessBackendError

/**
 * @param names
 * @param templateIdentifier resource path, file path, etc.
 *                           Whatever is recognized by the resource loader
 */
case class Backend(names: Set[String],
                   templateIdentifier: String) {
  def matches(x: String): Boolean =
    names.exists(Backend.areEqual(_, x))

  def toOutputStringName: Either[SqlppError, String] =
    names.headOption match {
      case Some(x) => Right(x)
      case None => Left(new NamelessBackendError(
        s"Backend with template identifier $templateIdentifier" +
          s" has no name"
      ))
    }
}

object Backend {
  class NamelessBackendError(override val msg: String)
    extends SqlppError(msg)

  def areEqual(left: String, right: String): Boolean = {
    def normalizeString(str: String): String = str.trim.toLowerCase()

    normalizeString(left) == normalizeString(right)
  }
}