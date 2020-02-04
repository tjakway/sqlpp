package com.jakway.sqlpp.config

trait CommonDefaults {
  object ProfileDir {
    val defaultRejectNonXMLFiles: Boolean = true
  }
}

object CommonDefaults extends CommonDefaults
