package com.jakway.sqlpp.util.env

trait SystemEnvReader[ErrorType]
  extends SystemValuesReader[ErrorType] {

  override val catchIllegalArgumentException: Boolean = false
  override protected def getSystemValue: String => String = System.getenv
}
