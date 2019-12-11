package com.jakway.sqlpp.util

import java.io._

import scala.util.{Failure, Success, Try}

object FileUtil {
  private def openBufferedOutputStreamWriter[ErrorType](
     encoding: String,
     errorF: Throwable => ErrorType)
    (openF: () => OutputStream):
    Either[ErrorType, Writer] = {

    Try(new BufferedWriter(
      new OutputStreamWriter(openF(), encoding))) match {
      case Success(x) => Right(x)
      case Failure(t) => Left(errorF(t))
    }
  }

  def openWriter[ErrorType](encoding: String, errorF: Throwable => ErrorType)
                (file: File): Either[ErrorType, Writer] = {
    openBufferedOutputStreamWriter[ErrorType](
      encoding, errorF)(() => new FileOutputStream(file))
  }

  def openPrintStreamWriter[ErrorType](encoding: String,
                                       errorF: Throwable => ErrorType)
                                      (printStream: PrintStream):
    Either[ErrorType, Writer] = {

    openBufferedOutputStreamWriter[ErrorType](
      encoding, errorF)(() => printStream)
  }
}
