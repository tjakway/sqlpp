package com.jakway.sqlpp.config.test.framework

import java.io.File
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Files

import com.jakway.sqlpp.config.test.error.TestError
import com.jakway.sqlpp.config.test.framework.TestFileUtils.{ReadEntireFileError, WriteToFileError}
import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import com.jakway.sqlpp.util.TryToEither

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

  /**
   * safely pass a Source to [[f]] before closing it
   * @param open
   * @param f
   * @tparam A
   * @return
   */
  def withSource[A](open: () => Source)(f: Source => A): A = {
    var optSource: Option[Source] = None
    try {
      val s = open()
      optSource = Some(s)
      f(s)
    } finally {
      optSource.foreach(_.close())
    }
  }


  def readEntireFile(f: File,
                     charset: Charset =
                       TestFileUtils.defaultCharset):
    Either[SqlppError, String] = {

    def open() = Source.fromFile(f, charset.displayName())

    def g: Try[String] = Try(withSource(open)(_.mkString))

    for {
      _ <- CheckFile.checkExists(f)
      _ <- CheckFile.checkReadable(f)
      res <- TryToEither(new ReadEntireFileError(f, _))(g)
    } yield {res}
  }
}

object TestFileUtils {
  class WriteToFileError(override val msg: String)
    extends TestError(msg) {

    def this(contents: String,
             dest: File,
             cause: Throwable) {
      this(s"Error writing < $contents > to file < $dest >: " +
        SqlppError.formatThrowable(cause))
    }
  }

  class ReadEntireFileError(override val msg: String)
    extends TestError(msg) {
    def this(f: File, cause: Throwable) {
      this(s"Error reading file < $f >: " +
        SqlppError.formatThrowableCause(cause))
    }
  }

  val defaultCharset: Charset = StandardCharsets.UTF_8
}
