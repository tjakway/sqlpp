package com.jakway.sqlpp.util.env

import com.jakway.sqlpp.util.PrintThrowable

import scala.util.{Try, Success, Failure}

trait PropertyReader[KeyType, ValueType, ErrorType] {
  protected def onKeyNotFound: String => Either[ErrorType, ValueType]
  protected def onInvalidKey: String => Throwable => Either[ErrorType, ValueType]
  protected def onOtherError: String => Throwable => Either[ErrorType, ValueType]

  def getPropertyF: KeyType => Either[ErrorType, ValueType]
}

trait StringPropertyReader[ErrorType]
  extends PropertyReader[String, String, ErrorType]

object StringPropertyReader {
  trait ErrorFormatter {
    protected val actionDesc: Option[String]

    private def header(key: String,
                       action: Option[String]): String = {
      val actionStr = action.map(s => " " + s).getOrElse("")
      s"Error reading key $key$actionStr: "
    }

    protected def mkErrorMsg(key: String,
                   msg: String): String = {
      header(key, actionDesc) + msg
    }

    protected def mkErrorMsg(key: String,
                   throwable: Throwable): String = {
      mkErrorMsg(key, PrintThrowable(throwable))
    }
  }
}

trait SystemValuesReader[ErrorType]
  extends StringPropertyReader[ErrorType] {

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
      case Success(None) => onKeyNotFound(key)
      case Failure(t) if badKeyError(t) => onInvalidKey(key)(t)
      case Failure(t) => onOtherError(key)(t)
    }
  }
}

trait SystemPropertyReader[ErrorType]
  extends SystemValuesReader[ErrorType] {

  override val catchIllegalArgumentException: Boolean = true
  override protected def getSystemValue: String => String = System.getProperty
}

trait SystemEnvReader[ErrorType]
  extends SystemValuesReader[ErrorType] {

  override val catchIllegalArgumentException: Boolean = false
  override protected def getSystemValue: String => String = System.getenv
}
