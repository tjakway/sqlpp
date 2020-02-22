package com.jakway.sqlpp.util

import java.io._
import java.nio.file.Files

import com.jakway.sqlpp.error.SqlppError

import scala.io.Source
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


  /**
   * see https://stackoverflow.com/questions/941272/how-do-i-trim-a-file-extension-from-a-string-in-java
   * @param f
   * @return
   */
  def nameWithoutExtension(f: File): String = {
    val s = f.getName
    if(s.contains(".")) {
      s.substring(0, s.lastIndexOf('.'))
    } else {
      s
    }
  }

  def readIntoString[ErrorType](
                   encoding: String, errorF: Throwable => ErrorType)(
                   is: InputStream): Either[ErrorType, String] = {
    def f = Try(Source.fromInputStream(is, encoding).mkString)

    TryToEither(errorF)(f)
  }

  def writeString[ErrorType](
                   encoding: String, errorF: Throwable => ErrorType)(
                   input: String, dest: File): Either[ErrorType, Unit] = {
    def writeF(writeTo: Writer): Either[ErrorType, Unit] = {
      val f = Try(writeTo.write(input))

      def closeF: Try[Unit] = {
        Try(writeTo.close())
      }

      val additionalMsg: String = s"could not close writer to $dest"

      TryToEither(errorF)(TryUtil.alwaysDoAfter(f, closeF, additionalMsg))
    }

    for {
      writer <- openWriter(encoding, errorF)(dest)
      res <- writeF(writer)
    } yield {
      res
    }
  }
}
