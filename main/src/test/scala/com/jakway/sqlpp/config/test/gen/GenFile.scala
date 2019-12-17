package com.jakway.sqlpp.config.test.gen

import java.io.File
import java.nio.file.Files

import com.jakway.sqlpp.config.test.TestException
import com.jakway.sqlpp.config.test.gen.GenFile.GenFileException
import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import com.jakway.sqlpp.error.CheckFile.FilePermissions
import org.scalacheck.Gen

import scala.util.Try

class GenFile(val tempDir: File) {

  def nameExists(name: String): Boolean =
    new File(tempDir, name).exists()

  def apply(genFilename: Gen[String],
            perms: FilePermissions): Gen[File] = {
    genFilename
      .suchThat(!nameExists(_))
      .map { name =>
        val f = new File(tempDir, name)
        perms.applyPermissions(f) match {
          case Right(_) => f
          case Left(error) =>
            throw new GenFileException(error)
        }
      }
  }
}

object GenFile {
  val defaultTempDirPrefix: String = "genfile_"

  class GenFileException(val error: SqlppError)
    extends TestException(error.msg)

  def getTempDir(prefix: String = defaultTempDirPrefix): Try[File] = Try {
    Files.createTempDirectory(prefix).toFile
  }

  def apply(prefix: String = defaultTempDirPrefix): Try[GenFile] = {
    getTempDir(prefix).map(new GenFile(_))
  }
}
