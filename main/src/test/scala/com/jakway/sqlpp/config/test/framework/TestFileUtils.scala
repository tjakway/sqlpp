package com.jakway.sqlpp.config.test.framework

import java.io.File
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Files

import com.jakway.sqlpp.config.test.framework.TestFileUtils.WriteToFileError
import com.jakway.sqlpp.error.SqlppError

import scala.io.Source
import scala.util.{Failure, Success, Try}

trait TestFileUtils {

  def writeToFile(str: String,
                  dest: File,
                  charset: Charset =
                    TestFileUtils.defaultCharset): Either[SqlppError, Unit] = {

    def err[A](cause: Throwable): Either[SqlppError, A] =
      Left(new WriteToFileError(str, dest, cause))

    def write: Either[SqlppError, Unit] = {
      Try(Files.write(
        dest.toPath, str.getBytes(charset))) match {
        case Success(_) => Right({})
        case Failure(t) => err(t)
      }
    }

    def read: Either[SqlppError, String] = {
      var toClose: Option[Source] = None
      val res = Try {
        val source =
          Source.fromFile(dest, charset.displayName())
        toClose = Some(source)
        source.mkString
      }

      toClose.foreach(_.close())
      res match {
        case Success(x) => Right(x)
        case Failure(t) => err(t)
      }
    }

    def check(readStr: String): Either[SqlppError, Unit] = {
      if(str == readStr) {
        Right({})
      } else {
        Left(new WriteToFileError(
          s"Expected to read back original string < $str > but " +
            s"got < $readStr >"))
      }
    }

    for {
      _ <- write
      readStr <- read
      _ <- check(readStr)
    } yield {}
  }

}

object TestFileUtils {
  class WriteToFileError(override val msg: String)
    extends SqlppError(msg) {

    def this(contents: String,
             dest: File,
             cause: Throwable) {
      this(s"Error writing < $contents > to file < $dest >: " +
        SqlppError.formatThrowable(cause))
    }
  }

  val defaultCharset: Charset = StandardCharsets.UTF_8
}
