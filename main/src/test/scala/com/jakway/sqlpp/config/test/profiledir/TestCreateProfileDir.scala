package com.jakway.sqlpp.config.test.profiledir

import java.io.File
import java.nio.file.Files

import com.jakway.sqlpp.config.entries.ParseOutputPattern
import com.jakway.sqlpp.config.output.StdoutOutputPattern
import com.jakway.sqlpp.config.test.{GenTestConfig, TestConfig, WithDefaultTestConfig}
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

  abstract class CreateProfileDirTest(val profDir: File,
                                      val backends: Set[Backend],
                                      val encoding: String,
                                      val deleteOnFailure: Boolean) {
    def checkOutcome: File =>
                      Either[SqlppError, Unit] =>
                      Either[SqlppError, Unit]
  }

  class CreateProfileDirSuccessTest(override val profDir: File,
                                    override val backends: Set[Backend],
                                    override val encoding: String,
                                    override val deleteOnFailure: Boolean)
    extends CreateProfileDirTest(profDir,
      backends, encoding, deleteOnFailure) {

    override def checkOutcome: File =>
      Either[SqlppError, Unit] =>
      Either[SqlppError, Unit] = {
      (profileDir: File) =>
        (createProfileDirRes: Either[SqlppError, Unit]) => {

          for {
            _ <- createProfileDirRes
            //TODO
          } yield {}
      }
    }
  }

  val defaultGenProfileDirPrefix: String = "genprofdir"

  def genCreateProfileDirSuccessTest(tempDir: File,
                                     prefix: String =
                                       defaultGenProfileDirPrefix,
                                     genEncoding: Gen[String] =
                                      GenTestConfig.genEncoding,
                                     genBackends: Gen[Set[Backend]]):
    Gen[CreateProfileDirSuccessTest] = {

    def genProfDir: Gen[File] = {
      val profDir = Files.createTempDirectory(tempDir.toPath, prefix)
      if(profDir.toFile.exists()) {
        Files.delete(profDir)
      }

      assert(!profDir.toFile.exists())
      profDir.toFile
    }


    val genDeleteOnFailure: Gen[Boolean] = Arbitrary.arbBool.arbitrary

    for {
      profDir <- genProfDir
      encoding <- genEncoding
      backends <- genBackends
      deleteOnFailure <- genDeleteOnFailure
    } yield {
      new CreateProfileDirSuccessTest(
        profDir, backends, encoding, deleteOnFailure)
    }
  }
}
