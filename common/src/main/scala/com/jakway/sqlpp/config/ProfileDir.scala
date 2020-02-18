package com.jakway.sqlpp.config

import java.io.{File, FileFilter}

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.backend.PropertiesFileBackend
import com.jakway.sqlpp.util.{FileUtil, TryToEither}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

class ProfileDir(val profileDirLocation: File,
                 val rejectNonXmlFiles: Boolean =
                  CommonDefaults.ProfileDir.defaultRejectNonXMLFiles) {
  import ProfileDir._
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  val backendsDir: File =
    new File(profileDirLocation, Constants.Profile.backendsDirName)

  def getBackends: Either[SqlppError, Set[PropertiesFileBackend]] = {
    if(backendsDir.exists()) {
      Left(new FindBackendFilesError(
        s"Expected to find backends in profile subdirectory " +
        backendsDir.getAbsolutePath + " but " +
        " it does not exist."))
    } else {
      def f: Try[Set[PropertiesFileBackend]] = Try {
        val backendFiles = backendsDir.listFiles(backendFilesFilter)
        backendFiles.toSet.map(f =>
          new PropertiesFileBackend(
            Set(FileUtil.nameWithoutExtension(f)), f))
      }

      TryToEither(new FindBackendFilesError(_))(f)
    }
  }

  private val backendFilesFilter: FileFilter = new FileFilter {
    override def accept(file: File): Boolean = {
      def warn(cause: String): Unit = {
        logger.warn("Expected only XML files but found " +
          cause + " in backends dir < " +
          backendsDir.getAbsolutePath + ">, ignoring")
      }

      if(file.isDirectory) {
        logger.warn("directory " + file.toString)
        false
      } else {
        val isXMLFile: Boolean = file.toString.endsWith(".xml")
        if(isXMLFile) {
          true
        } else {
          if(rejectNonXmlFiles) {
            logger.warn("non-xml file " + file.toString)
            false
          } else {
            logger.info("Allowing non-XML file because rejectNonXMLFiles is set")
            true
          }
        }
      }
    }
  }
}

object ProfileDir {
  class ProfileDirError(override val msg: String)
    extends SqlppError(msg)

  class FindBackendFilesError(override val msg: String)
    extends SqlppError(msg) {
    def this(throwable: Throwable) {
      this(SqlppError.formatThrowableCause(throwable))
    }
  }
}
