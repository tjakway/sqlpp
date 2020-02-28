package com.jakway.sqlpp.config.checked.profiledir

import java.io.{File, InputStream}

import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import com.jakway.sqlpp.template.backend.Backend
import com.jakway.sqlpp.util.{FileUtil, TryToEither}
import com.jakway.sqlpp.config.checked.profiledir.errors._

import scala.util.Try

object CreateProfileDir {
  private def copyBackend(backend: Backend,
                          to: File,
                          encoding: String): Either[SqlppError, Unit] = {
    def close(x: InputStream): Either[SqlppError, Unit] = {
      def onErrorF: Throwable => SqlppError = { (t: Throwable) =>
        new CreateProfileDirFileOperationError(
          s"Error closing InputStream corresponding to backend $Backend: " +
            SqlppError.formatThrowableCause(t))
      }

      def f: Try[Unit] = Try(x.close())
      TryToEither(onErrorF)(f)
    }

    val is = backend.read

    def errorF = CreateProfileDirFileOperationError.apply _

    //read the backend into a string then write it out to the passed file
    val res = for {
      is <- backend.read
      backendContents <- FileUtil.readIntoString(
        encoding, errorF)(is)
      _ <- FileUtil.writeString(encoding, errorF)(backendContents, to)
    } yield {}

    is.map(close).flatMap(ignored => res)
  }

  private def copyBackends(backends: Map[Backend, File],
                           encoding: String): Either[SqlppError, Unit] = {

    val zero: Either[SqlppError, Unit] = Right({})
    backends.foldLeft(zero) {
      case (eAcc, (backend, dest)) => eAcc.flatMap { ignored =>
        copyBackend(backend, dest, encoding)
      }
    }
  }

  private def assignBackendDests(backends: Set[Backend],
                                 in: File): Either[SqlppError, Map[Backend, File]] = {
    val zero: Either[SqlppError, Map[Backend, File]] = Right(Map.empty)

    backends.foldLeft(zero) {
      case (eAcc, thisBackend) => eAcc.flatMap { acc =>
        thisBackend
          .getFileInDir(in)
          .map(res => acc.updated(thisBackend, res))
      }
    }
  }

  /**
   *
   * @param dest
   * @param deleteOnFailure whether to remove the directory if profile creation fails for any reason
   *                        except that the profile directory already exists
   */
  def createProfileDir(backends: Set[Backend],
                       dest: File,
                       encoding: String,
                       deleteOnFailure: Boolean):
  Either[SqlppError, Unit] = {

    if(dest.exists()) {
      Left(new ProfileDirAlreadyExistsError(
        dest))
    } else {
      val res = if(dest.mkdirs()) {

        for {
          backendsWithDests <- assignBackendDests(backends, dest)
          _ <- copyBackends(backendsWithDests, encoding)
          _ <- checkProfileDirPermissions(dest)
        } yield {}
      } else {
        Left(new CreateProfileDirFileOperationError(
          s"Failed to create config directory $dest"))
      }

      res match {
        //DeleteProfileDirError will report the original error
        //as well as any error caused by the deletion
        case Left(e) if deleteOnFailure && dest.exists() => {
          def errWithMsg: String => SqlppError =
            new DeleteProfileDirError(dest, _, e)

          def errorF: Throwable => SqlppError = (t: Throwable) =>
            errWithMsg(SqlppError.formatThrowable(t))

          FileUtil.recursivelyDelete(dest, errorF, errWithMsg) match {
            case Right(_) => res
            case q@Left(_) => q
          }
        }
        case _ => res
      }
    }
  }

  /**
   * TODO: add checks for the contents of the directory instead of just
   * the directory itself
   * @param dir
   * @return
   */
  def checkProfileDirPermissions(dir: File): Either[SqlppError, Unit] = {
    val checks = Seq(
      CheckFile.checkExists,
      CheckFile.checkIsDirectory,
      CheckFile.checkReadable,
      CheckFile.checkExecutable,
    )

    CheckFile.composeAll(checks)(dir)
  }
}
