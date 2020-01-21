package com.jakway.sqlpp.config

import enumeratum._

sealed trait VerbosityLevel extends EnumEntry

object VerbosityLevel extends Enum[VerbosityLevel] {
  val values = findValues

  case object Verbose extends VerbosityLevel
  case object Standard extends VerbosityLevel
  case object Quiet extends VerbosityLevel
}

