package com.jakway.sqlpp.util.env

import com.jakway.sqlpp.util.PrintThrowable

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