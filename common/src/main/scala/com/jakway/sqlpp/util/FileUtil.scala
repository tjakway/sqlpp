package com.jakway.sqlpp.util

import java.io._
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitOption, FileVisitResult, FileVisitor, Files, Path, SimpleFileVisitor}

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

  private class CanDeleteVisitor[A <: Path]
    extends SimpleFileVisitor[A] {
    import java.util.{Set => JSet, HashSet => JHashSet}
    import java.util.{Collections => JCollections}

    val lackingPermissions: JSet[A] =
      JCollections.synchronizedSet(new JHashSet[A]())


    def canDeleteDirectoryPermissions(d: File): Boolean = {
      d.canExecute && d.canRead && d.canWrite
    }

    def canDeleteFilePermissions(f: File): Boolean = {
      f.canWrite
    }

    override def preVisitDirectory(d: A,
                                   basicFileAttributes: BasicFileAttributes):
      FileVisitResult = {

      //add the directory itself to our list without adding individual files
      if(canDeleteDirectoryPermissions(d.toFile)) {
        FileVisitResult.CONTINUE
      } else {
        lackingPermissions.add(d)
        FileVisitResult.SKIP_SUBTREE
      }
    }

    override def visitFile(f: A, basicFileAttributes: BasicFileAttributes): FileVisitResult = {
      //record & keep going
      if(!canDeleteFilePermissions(f.toFile)) {
        lackingPermissions.add(f)
      }

      FileVisitResult.CONTINUE
    }
  }

  /**
   *
   * @param d
   * @return the list of files or directories we don't have the correct permissions
   *         to delete
   */
  def getDirectoryDeletionPermissionErrors(d: File): Set[File] = {
    val visitor = new CanDeleteVisitor[Path]()
    Files.walkFileTree(d.toPath, visitor)
    scala.collection.JavaConverters
      .asScalaSet(visitor.lackingPermissions)
      .map(_.toFile)
      .toSet
  }

  private def getDeleteVisitor[A <: Path]: FileVisitor[A] = new SimpleFileVisitor[A] {
    override def visitFile(f: A,
                           basicFileAttributes: BasicFileAttributes): FileVisitResult = {

      Files.delete(f)
      FileVisitResult.CONTINUE
    }

    override def postVisitDirectory(d: A,
                                    e: IOException): FileVisitResult = {
      //rethrow any exceptions that occurred
      Option(e).foreach(x => throw x)

      //otherwise continue
      Files.delete(d)
      FileVisitResult.CONTINUE
    }
  }

  def recursivelyDelete[ErrorType](f: File,
                                   errorF: Throwable => ErrorType,
                                   errorMessageF: String => ErrorType):
    Either[ErrorType, Unit] = {

    def checkCanDelete: Either[ErrorType, Unit] = {
      def formatMsg(fs: Set[File]): String = {
        import java.util.Formatter
        val fmt = {
          val sb = new StringBuffer()
          new Formatter(sb)
        }

        fmt.format("Lacking permission to delete the following files (")
        fmt.format("no action taken):\n")
        fs.foreach { thisFile =>
          fmt.format("\t%s\n", thisFile)
        }
        fmt.toString.trim
      }

      val lackingPermissions = getDirectoryDeletionPermissionErrors(f)

      if(lackingPermissions.isEmpty) {
        Right({})
      } else {
        Left(errorMessageF(formatMsg(lackingPermissions)))
      }
    }

    def tryDelete =
      TryToEither(errorF)(Try(Files.walkFileTree(f.toPath, getDeleteVisitor)))

    for {
      _ <- checkCanDelete
      _ <- tryDelete
    } yield {}
  }
}
