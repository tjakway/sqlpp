package com.jakway.sqlpp.config.test.framework

import java.io.File
import java.nio.file.Files

import com.jakway.sqlpp.config.test.error.TestError
import com.jakway.sqlpp.config.test.profiledir.CreateProfileDirProperties.CreateProfileDirTestException
import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import com.jakway.sqlpp.util.FileUtil
import org.slf4j.{Logger, LoggerFactory}


trait WithTempDir {
  import WithTempDir._

  private val logger: Logger = LoggerFactory.getLogger(getClass)
  private var tempDir: Option[File] = None

  protected def tempDirPrefix: String = defaultTempDirPrefix

  protected def checkDir(dir: File): Either[SqlppError, Unit] = {
    val checks = Seq(
      CheckFile.checkExists,
      CheckFile.checkIsDirectory,
      CheckFile.setExecutable(true),
      CheckFile.setReadable(true),
      CheckFile.setWritable(true))

    CheckFile.composeAll(checks)(dir)
  }

  protected def mkTempDir(): Unit = {
    synchronized {
      tempDir = Some(
        Files.createTempDirectory(tempDirPrefix).toFile)

      tempDir.foreach { d =>
        if(!d.exists()) {
          d.mkdir()
        }

        checkDir(d).right.get
      }

      assert(tempDir.isDefined)
    }
  }

  protected def getTempDir: File = {
    synchronized {
      tempDir.getOrElse(
        throw new CreateProfileDirTestException("tempDir is null"))
    }
  }

  protected def rmTempDir(): Unit = {
    synchronized {
      if(tempDir.isEmpty) {
        logger.warn("Warning: rmTempDir() called with tempDir=None")
      }

      tempDir.foreach { d =>
        if(d.exists()) {
          //throw on error
          FileUtil.recursivelyDelete(d, errorF, errorMessageF)
            .right.get
        }
      }

      tempDir = None
    }
  }
}

object WithTempDir {
  val defaultTempDirPrefix: String = "sqlpp"

  class WithTempDirError(override val msg: String)
    extends TestError(msg) {
    def this(throwable: Throwable) {
      this(SqlppError.formatThrowableCause(throwable))
    }
  }

  private def errorF: Throwable => SqlppError = new WithTempDirError(_)
  private def errorMessageF: String => SqlppError = new WithTempDirError(_)
}
