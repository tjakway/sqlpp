package com.jakway.sqlpp.config.checked

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.nio.file.Files

import com.jakway.sqlpp.config.Constants
import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.config.output.StandardBackendResources
import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import com.jakway.sqlpp.util.TryToEither

import scala.util.Try

case class CreateConfigDir(dest: File,
                           additionalBackends: Set[File] = Set.empty) {

  lazy val templatesDir: File = new File(dest, Constants.templatesDirName)

  private def mkdir(x: File): Either[SqlppError, Unit] = {
    val fs: Seq[CheckFile.FileCheckF] =
      Seq(CheckFile.mkDir,
        CheckFile.setWritable(true),
        CheckFile.setReadable(true),
        CheckFile.setExecutable(true))

    CheckFile.composeAll(fs)(x)
  }

  def mkdirs(): Either[SqlppError, Unit] = {
    for {
      _ <- mkdir(dest)
      _ <- mkdir(templatesDir)
    } yield {}
  }
}


object CreateConfigDir {
  class CreateConfigDirError(override val msg: String)
    extends ConfigError(msg) {
    def this(throwable: Throwable) {
      this(SqlppError.formatThrowableCause(throwable))
    }
  }

  class CopyBackendToConfigDirError(val resourceDescription: String,
                                    val dest: File,
                                    val cause: Throwable)
    extends CreateConfigDirError(
      s"Error copying $resourceDescription to $dest caused by: " +
      SqlppError.formatThrowable(cause))


  private def allStreams(createConfigDir: CreateConfigDir):
    Either[SqlppError, Set[(String, File, InputStream)]] = {

    def mkDest(fileName: String): File =
      new File(createConfigDir.templatesDir, fileName)

    def zero: Either[SqlppError, Set[(String, File, InputStream)]] =
      Right(Set.empty)

    def additionalBackendStreams:
    Either[SqlppError, Set[(String, File, InputStream)]] = {

      createConfigDir.additionalBackends.foldLeft(zero) {
        case (eAcc, additionalBackend) => eAcc.flatMap { acc =>
          TryToEither(new CreateConfigDirError(_)) {
            Try(
              additionalBackend.getAbsolutePath,
              mkDest(additionalBackend.getName),
              new BufferedInputStream(
                new FileInputStream(additionalBackend)))
          }.map(res => acc + res)
        }
      }
    }


    def standardBackendResourceStreams:
    Either[SqlppError, Set[(String, File, InputStream)]] = {

      StandardBackendResources.getInputStreamPairs.flatMap { pairs =>
        pairs.foldLeft(zero) {
          case (eAcc, (resName, resourceStream)) => eAcc.map { acc =>
            acc.+((resName, mkDest(resName), resourceStream))
          }
        }
      }
    }

    for {
      xs <- additionalBackendStreams
      ys <- standardBackendResourceStreams
    } yield {
      xs ++ ys
    }
  }

  def apply(createConfigDir: CreateConfigDir): Either[SqlppError, Unit] = {
    for {
      //create the config dir we're going to copy backend files into
      _ <- createConfigDir.mkdirs()
      streams <- allStreams(createConfigDir)
    } yield {
      //copy each backend file to its location in the config dir
      val zero: Either[SqlppError, Unit] = Right({})
      streams.foldLeft(zero) {
        case (eAcc, (inputDescription, dest, inputStream)) =>
          eAcc.flatMap { ignored =>
            val copyRes = TryToEither(
              new CopyBackendToConfigDirError(
                inputDescription,
                dest,
                _)) {
              Try(Files.copy(inputStream, dest.toPath))
            }

            for {
              _ <- copyRes
              //make sure the file we just copied is readable
              //or there wasn't much point
              _ <- CheckFile.setReadable(true)(dest)
            } yield {}
          }
      }
    }
  }
}