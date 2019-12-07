package com.jakway.sqlpp.error

import java.io.File

import com.jakway.sqlpp.error.CheckFile.FileError.{CannotReadError, CannotWriteError, FileDoesNotExistError, NotDirectoryError, NotFileError}

object CheckFile {
  class FileError(override val msg: String)
    extends SqlppError(msg)

  class FileErrors(val errors: Seq[FileError])
    extends SqlppError(SqlppError.formatErrors(errors))

  object FileError {
    class FileDoesNotExistError(val f: File)
      extends FileError(s"$f does not exist")

    class OperationError(val f: File,
                         override val msg: String)
      extends FileError(msg)

    class CannotReadError(override val f: File)
      extends OperationError(f, s"Cannot read file $f")

    class CannotWriteError(override val f: File)
      extends OperationError(f, s"Cannot write to file $f")

    class UnexpectedFileTypeError(val f: File,
                                  val expectedType: String,
                                  val actual: String)
      extends FileError(s"Expected $f to be $expectedType but it's $actual")

    class NotFileError(override val f: File)
      extends UnexpectedFileTypeError(f, "a file", "a directory")

    class NotDirectoryError(override val f: File)
      extends UnexpectedFileTypeError(f, "a directory", "a file")
  }

  type FileCheckF = File => Either[SqlppError, Unit]

  private def mkCheck(checkF: File => Boolean)
                     (errorConstructor: File => SqlppError): FileCheckF = {
    (f: File) =>
      if(!checkF(f)) {
        Left(errorConstructor(f))
      } else {
        Right({})
      }
  }

  def checkExists: FileCheckF = mkCheck(_.exists)(new FileDoesNotExistError(_))
  def checkReadable: FileCheckF = mkCheck(_.canRead)(new CannotReadError(_))
  def checkWriteable: FileCheckF = mkCheck(_.canWrite)(new CannotWriteError(_))
  def checkIsFile: FileCheckF = mkCheck(_.isFile)(new NotFileError(_))
  def checkIsDirectory: FileCheckF = mkCheck(_.isDirectory)(new NotDirectoryError(_))
}
