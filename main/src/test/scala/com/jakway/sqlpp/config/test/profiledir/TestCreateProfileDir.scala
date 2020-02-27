package com.jakway.sqlpp.config.test.profiledir

import java.io.File

import com.jakway.sqlpp.config.entries.ParseOutputPattern
import com.jakway.sqlpp.config.output.StdoutOutputPattern
import com.jakway.sqlpp.config.test.WithDefaultTestConfig
import com.jakway.sqlpp.config.test.error.TestError
import com.jakway.sqlpp.config.test.gen.GenUtil
import com.jakway.sqlpp.config.test.profiledir.CreateProfileDirProperties.CreateProfileDirTestException
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.backend.Backend
import com.jakway.sqlpp.util.FileUtil
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.slf4j.{Logger, LoggerFactory}

class CreateProfileDirProperties
  extends AnyPropSpec
    with Matchers
    with WithDefaultTestConfig {
  import ScalaCheckPropertyChecks._
}

object CreateProfileDirProperties {
  class CreateProfileDirTestException(val msg: String)
    extends RuntimeException(msg)

  //TODO
  def testCleanUp(tempDir: File): Unit = ???

  abstract class CreateProfileDirTest(val backends: Set[Backend],
                                      val encoding: String,
                                      val deleteOnFailure: Boolean) {
    def checkOutcome: File =>
                      Either[SqlppError, Unit] =>
                      Either[SqlppError, Unit]
  }

  class CreateProfileDirSuccessTest(override val backends: Set[Backend],
                                    override val encoding: String,
                                    override val deleteOnFailure: Boolean)
    extends CreateProfileDirTest(backends, encoding, deleteOnFailure) {

    override def checkOutcome: File =>
      Either[SqlppError, Unit] =>
      Either[SqlppError, Unit] = {
      (profileDir: File) =>
        (createProfileDirRes: Either[SqlppError, Unit]) => {

          for {
            _ <- createProfileDirRes

          } yield {}
      }
    }
  }

  trait WithTempDir {
    import WithTempDir._

    private val logger: Logger = LoggerFactory.getLogger(getClass)
    private var tempDir: Option[File] = None

    protected def tempDirPrefix: String = defaultTempDirPrefix

    private def formatTempDirTemplateString(prefix: String,
                                            minimumXs: Int): String = {
      assert(minimumXs > 0)

      val templateEnding: String = "." +
        (1 to minimumXs).map(_ => "X")

      if(prefix.endsWith(templateEnding)) {
        prefix
      } else {
        prefix + templateEnding
      }
    }

    protected def mkTempDir(): Unit = {
      synchronized {
        //TODO
        tempDir = Some(???)
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
    val defaultTempDirPrefix: String = "sqlpp_with_temp_dir"

    class WithTempDirError(override val msg: String)
      extends TestError(msg) {
      def this(throwable: Throwable) {
        this(SqlppError.formatThrowableCause(throwable))
      }
    }

    private def errorF: Throwable => SqlppError = new WithTempDirError(_)
    private def errorMessageF: String => SqlppError = new WithTempDirError(_)
  }
}
