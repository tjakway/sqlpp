package com.jakway.sqlpp.util.env

import scala.util.{Failure, Success, Try}

trait SystemValuesReader[ErrorType]
  extends StringPropertyReader[ErrorType]
    with StringPropertyReader.ErrorFormatter {

  /**
   * @param t caused by
   * @return
   */
  protected def badKeyError(t: Throwable): Boolean = t match {
    case _: NullPointerException => true
    case _: IllegalArgumentException
      if catchIllegalArgumentException => true

    case _ => false
  }

  /**
   * javadoc lists that this can be
   * thrown for getProperty
   * but not for getenv
   * see https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/System.html#getenv(java.lang.String)
   */
  protected val catchIllegalArgumentException: Boolean
  protected def getSystemValue: String => String

  override def getPropertyF: String => Either[ErrorType, String] = { key =>
    //see https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/System.html#getProperty(java.lang.String)
    Try(Option(getSystemValue(key))) match {
      case Success(Some(foundValue)) => Right(foundValue)
      case Success(None) => onKeyNotFound(mkErrorMsg(key, "key not found"))
      case Failure(t) if badKeyError(t) => onInvalidKey(mkErrorMsg(key, t))
      case Failure(t) => onOtherError(mkErrorMsg(key, t))
    }
  }
}
