package com.jakway.sqlpp.util.env

trait PropertyReader[KeyType, ValueType, ErrorType] {
  protected def onKeyNotFound: String => Either[ErrorType, ValueType]
  protected def onInvalidKey: String =>  Either[ErrorType, ValueType]
  protected def onOtherError: String =>  Either[ErrorType, ValueType]

  def getPropertyF: KeyType => Either[ErrorType, ValueType]
}
