package com.jakway.sqlpp.error

import java.io.{File, Writer}

import com.jakway.sqlpp.error.CheckFile.FileError.{CannotExecuteError, CannotReadError, CannotWriteError, ExceptionThrownDuringOperationError, FileDoesNotExistError, MkdirError, NotDirectoryError, NotFileError}

import scala.util.{Failure, Success, Try}

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

    class ExceptionThrownDuringOperationError(override val f: File,
                                              val throwable: Throwable)
      extends OperationError(f,
        s"Caught throwable: " + SqlppError.formatThrowable(throwable))

    object ExceptionThrownDuringOperationError {
      def apply(f: File, t: Throwable): ExceptionThrownDuringOperationError =
        new ExceptionThrownDuringOperationError(f, t)
    }

    class CannotReadError(override val f: File)
      extends OperationError(f, s"Cannot read file $f")

    class CannotWriteError(override val f: File)
      extends OperationError(f, s"Cannot write to file $f")

    class CannotExecuteError(override val f: File)
      extends OperationError(f, s"Cannot execute $f")

    class UnexpectedFileTypeError(val f: File,
                                  val expectedType: String,
                                  val actual: String)
      extends FileError(s"Expected $f to be $expectedType but it's $actual")

    class NotFileError(override val f: File)
      extends UnexpectedFileTypeError(f, "a file", "a directory")

    class NotDirectoryError(override val f: File)
      extends UnexpectedFileTypeError(f, "a directory", "a file")

    class MkdirError(override val f: File)
      extends OperationError(f, s"$f.mkdir returned false")

    class SetPermissionError(override val f: File,
                             val setTo: Boolean,
                             val permissionVerb: String)
      extends OperationError(f,
        SetPermissionError.formatMsg(f, setTo, permissionVerb))

    object SetPermissionError {
      private def formatMsg(f: File,
                            setTo: Boolean,
                            permissionVerb: String): String = {
        val permString: String =
          if(setTo) {
            ""
          } else {
            "non "
          }

        s"Could not set f to $permString$permissionVerb"
      }

      class SetReadableError(override val f: File,
                             override val setTo: Boolean)
        extends SetPermissionError(f, setTo, "readable")

      class SetWritableError(override val f: File,
                              override val setTo: Boolean)
        extends SetPermissionError(f, setTo, "writeable")

      class SetExecutableError(override val f: File,
                               override val setTo: Boolean)
        extends SetPermissionError(f, setTo, "executable")
    }
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
  def checkWritable: FileCheckF = mkCheck(_.canWrite)(new CannotWriteError(_))
  def checkExecutable: FileCheckF = mkCheck(_.canExecute)(new CannotExecuteError(_))
  def checkIsFile: FileCheckF = mkCheck(_.isFile)(new NotFileError(_))
  def checkIsDirectory: FileCheckF = mkCheck(_.isDirectory)(new NotDirectoryError(_))

  private def operationReturnsBoolean(idempotentCondition: File => Boolean,
                                      op: File => Boolean)
                                     (ifFalse: File => SqlppError): FileCheckF = f => {
    val tryRes = Try {
      if(idempotentCondition(f)) {
        true
      } else {
        op(f)
      }
    }

    tryRes match {
      case Success(x) => {
        //op returns true on success, false on failure
        if(x) {
          Right({})
        } else {
          Left(ifFalse(f))
        }
      }
      case Failure(t) => Left(
        new ExceptionThrownDuringOperationError(f, t))
    }
  }

  import com.jakway.sqlpp.error.CheckFile.FileError.SetPermissionError._

  def mkDir: FileCheckF = operationReturnsBoolean(_.isDirectory, _.mkdir())(
    new MkdirError(_))

  def setReadable(b: Boolean): FileCheckF =
    operationReturnsBoolean(_.canRead, _.setReadable(b))(
      new SetReadableError(_, b))

  def setWritable(b: Boolean): FileCheckF =
    operationReturnsBoolean(_.canWrite, _.setWritable(b))(
      new SetWritableError(_, b))

  def setExecutable(b: Boolean): FileCheckF =
    operationReturnsBoolean(_.canExecute, _.setExecutable(b))(
      new SetExecutableError(_, b))

  def openWriter(open: File => Writer,
                 onError: File => Throwable => SqlppError =
                    ExceptionThrownDuringOperationError.apply _)
                (f: File): Either[SqlppError, Writer] = {
    Try(open(f)) match {
      case Success(x) => Right(x)
      case Failure(t) => Left(onError(f)(t))
    }
  }

}
