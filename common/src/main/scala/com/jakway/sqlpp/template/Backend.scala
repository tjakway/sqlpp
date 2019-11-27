package com.jakway.sqlpp

/**
 * @param names
 * @param templateIdentifier resource path, file path, etc.
 *                           Whatever is recognized by the resource loader
 */
case class Backend(names: Set[String],
                   templateIdentifier: String) {
  def matches(x: String): Boolean =
    names.exists(Backend.areEqual(_, x))
}

object Backend {
  def areEqual(left: String, right: String): Boolean = {
    def normalizeString(str: String): String = str.trim.toLowerCase()

    normalizeString(left) == normalizeString(right)
  }
}