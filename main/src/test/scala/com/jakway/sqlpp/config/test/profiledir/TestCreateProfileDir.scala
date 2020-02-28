package com.jakway.sqlpp.config.test.profiledir

import java.io.File
import java.nio.file.Files
import java.util.Formatter

import com.jakway.sqlpp.config.test.error.TestError
import com.jakway.sqlpp.config.test.profiledir.CreateProfileDirProperties._
import com.jakway.sqlpp.config.test.testconfig.{GenTestConfig, PrintConfig, WithDefaultTestConfig}
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.backend.Backend
import com.jakway.sqlpp.util.TryToEither
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Try

class CreateProfileDirProperties
  extends AnyPropSpec
    with Matchers
    with WithDefaultTestConfig {
  import GenCreateProfileDirTest._
  import ScalaCheckPropertyChecks._

  property("Make sure we can read our own test failure error messages") {
    forAll(genCreateProfileDirFailureTest) { test =>
      class FailureTestToStringException(override val msg: String)
        extends CreateProfileDirTestException(msg) {
        def this(throwable: Throwable) {
          this(SqlppError.formatThrowableCause(throwable))
        }
      }

      TryToEither(new FailureTestToStringException(_))(
        Try(test.toString)) should be ('right)
    }
  }
}

object CreateProfileDirProperties {
  class CreateProfileDirTestError(override val msg: String)
    extends TestError(msg)

  class CreateProfileDirTestException(val msg: String)
    extends RuntimeException(msg)

  class ExpectedDifferentOutcomeError(override val msg: String)
    extends CreateProfileDirTestError(msg)

  object ExpectedDifferentOutcomeError {
    def apply(msg: String, expected: String, actual: String):
      ExpectedDifferentOutcomeError = {

      val fmt: Formatter = {
        val sb = new StringBuffer()
        new Formatter(sb)
      }
      fmt.format("%s; expected: %s, actual: %s",
        msg, expected, actual)

      new ExpectedDifferentOutcomeError(fmt.toString)
    }
  }


  type CheckOutcomeF = File =>
    Either[SqlppError, Unit] =>
      Either[SqlppError, Unit]

  abstract class CreateProfileDirTest(val profDir: File,
                                      val backends: Set[Backend],
                                      val encoding: String,
                                      val deleteOnFailure: Boolean) {
    def checkOutcome: CheckOutcomeF
  }

  final class CreateProfileDirSuccessTest(
                                    override val profDir: File,
                                    override val backends: Set[Backend],
                                    override val encoding: String,
                                    override val deleteOnFailure: Boolean)
    extends CreateProfileDirTest(profDir,
      backends, encoding, deleteOnFailure) {

    override def checkOutcome: CheckOutcomeF = {
      (profileDir: File) =>
        (createProfileDirRes: Either[SqlppError, Unit]) => {

          for {
            _ <- createProfileDirRes

            //TODO
          } yield {}
        }
    }
  }

  class CreateProfileDirFailureTest(
                                 override val profDir: File,
                                 override val backends: Set[Backend],
                                 override val encoding: String,
                                 override val deleteOnFailure: Boolean,
                                 val checkOutcomeF: CheckOutcomeF,
                                 val testName: String)
    extends CreateProfileDirTest(
      profDir, backends, encoding, deleteOnFailure) {

    override def checkOutcome: CheckOutcomeF = checkOutcomeF

    override def toString: String = {
      val fmt: Formatter = {
        val sb = new StringBuffer()
        new Formatter(sb)
      }

      val tab: String = PrintConfig.standardTab

      //omit checkOutcomeF because functions never have meaningful toStrings
      val fmtString: String =
        "%s(\n%sprofDir = %s,\n%sbackends = %s," +
          "\n%sencoding = %s,\n%sdeleteOnFailure = %s," +
          "\n%sCheckOutcomeF,\n%stestName = %s)"

      fmt.format(fmtString,
        getClass.getName, tab,
        profDir.getCanonicalPath, tab,
        backends, tab,
        encoding, tab,
        deleteOnFailure, tab,
        testName, tab)


      fmt.toString
    }
  }

  object CreateProfileDirFailureTest {

    def fromSuccessTest(
       createProfileDirSuccessTest: CreateProfileDirSuccessTest)
       (newCheckFunction: CheckOutcomeF,
        testName: String): CreateProfileDirFailureTest = {

      val t = createProfileDirSuccessTest
      new CreateProfileDirFailureTest(
        t.profDir,
        t.backends,
        t.encoding,
        t.deleteOnFailure,
        newCheckFunction,
        testName)
    }
  }
}

object GenCreateProfileDirTest {

  val defaultGenProfileDirPrefix: String = "genprofdir"

  class Options(val tempDir: File,
                val prefix: String = defaultGenProfileDirPrefix,
                val genEncoding: Gen[String] =
                 GenTestConfig.genEncoding,
                val genBackends: Gen[Set[Backend]])

  def genCreateProfileDirSuccessTest(options: Options):
    Gen[CreateProfileDirSuccessTest] =
    genCreateProfileDirSuccessTest(
      options.tempDir, options.prefix, options.genEncoding, options.genBackends)

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

  private class GenCreateProfileDirFailureTest(val options: Options) {
    def expectError(successTest: CreateProfileDirSuccessTest)
                   (expectedErrorType: Class[_],
                    testName: String): CreateProfileDirFailureTest = {

      def errActual(msg: String,
              actual: Either[SqlppError, Unit]): Either[SqlppError, Unit] =
        Left(ExpectedDifferentOutcomeError(
          msg, expectedErrorType.getName, actual.toString))

      def checkF: CheckOutcomeF = file => outcome => {
        def err(msg: String):
          Either[SqlppError, Unit] = errActual(msg, outcome)

        outcome match {
          case Right(_) => err("Expected a Left")
          case Left(c) if c.getClass == expectedErrorType => Right({})
          case Left(_) => err("Left has wrong class")
        }
      }

      CreateProfileDirFailureTest
        .fromSuccessTest(successTest)(checkF, testName)
    }

    //TODO
    def apply: Gen[GenCreateProfileDirFailureTest] = ???
  }

  //TODO
  def genCreateProfileDirFailureTest: Gen[GenCreateProfileDirFailureTest] = ???
}
