package com.jakway.sqlpp

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