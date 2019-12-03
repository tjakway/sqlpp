package com.jakway.sqlpp.util.env

trait SystemPropertyReader[ErrorType]
  extends SystemValuesReader[ErrorType] {

  override val catchIllegalArgumentException: Boolean = true
  override protected def getSystemValue: String => String = System.getProperty
}
