package com.jakway.sqlpp.util

import java.io._

import scala.util.{Failure, Success, Try}

object FileUtil {
  def openWriter[ErrorType](encoding: String, errorF: Throwable => ErrorType)
                (file: File): Either[ErrorType, Writer] = {
    Try(new BufferedWriter(
      new OutputStreamWriter(
        new FileOutputStream(file), encoding))) match {
      case Success(x) => Right(x)
      case Failure(t) => Left(errorF(t))
    }
  }
}
