package com.jakway.sqlpp.config.test.util

trait StringCmpFunction {
  def apply(left: String, right: String): Boolean
}

object StringCmpFunction {
  object StrictEquality extends StringCmpFunction {
    override def apply(left: String, right: String): Boolean =
      left == right
  }
}