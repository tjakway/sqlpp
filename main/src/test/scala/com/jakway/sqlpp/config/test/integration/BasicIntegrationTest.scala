package com.jakway.sqlpp.config.test.integration

import java.io.File
import java.util.Formatter

import com.jakway.sqlpp
import com.jakway.sqlpp.Run
import com.jakway.sqlpp.config.test.error.TestError
import com.jakway.sqlpp.config.test.framework.{TestFileUtils, WithTempDirBeforeAndAfter}
import com.jakway.sqlpp.config.test.integration.BasicIntegrationTest.UnexpectedOutputError
import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.{Logger, LoggerFactory}

class BasicIntegrationTest
  extends AnyFlatSpec
    with Matchers
    with WithTempDirBeforeAndAfter
    with TestFileUtils{
  val logger: Logger = LoggerFactory.getLogger(getClass)

  def mkArgs(input: File, outputDest: File): Array[String] = Array(
    "--no-create-profile-dir",
    "--resource-loader-types", "file,class",
    "--source", input.getAbsolutePath,
    "--output", outputDest.getAbsolutePath,
    "--target-backends", "defaults.xml"
  )

  val input: String = "CREATE TABLE Users(" +
    "id $INDEX_TYPE_PK_AUTOINC," +
    "name $TEXT_TYPE)"

  val replacementMap: Map[String, String] = Map(
    "$TEXT_TYPE" -> "VARCHAR(65535)",
    "$INDEX_TYPE_PK_AUTOINC" -> "INTEGER PRIMARY KEY AUTOINCREMENT"
  )

  def mkExpectedOutput(kvs: Map[String, String])(input: String): String = {
    kvs.foreach {
      case (_, thisValue) => {
        if(kvs.contains(thisValue)) {
          logger.warn("Replacement value is itself" +
            " a key in the unordered replacement map--" +
            "this may result in unstable changes.")
        }
      }
    }

    kvs.foldLeft(input) {
      case (acc, (thisKey, thisValue)) => {
        acc.replaceAll(thisKey, thisValue)
      }
    }
  }

  def checkOutput(outputDest: File,
                  expectedOutput: String): Either[SqlppError, Unit] = {
    readEntireFile(outputDest).flatMap { contents =>

      if(contents == expectedOutput) {
        Right({})
      } else {
        Left(new UnexpectedOutputError(contents, expectedOutput))
      }
    }
  }

  private val inputFileChecks = Seq(
    CheckFile.checkExists,
    CheckFile.checkIsFile,
    CheckFile.checkReadable,
    CheckFile.checkNonEmpty
  )

  private val outputFileChecks = inputFileChecks


  "Sqlpp" should "run" in {
    val td = getTempDir

    val inputDest: File = new File(td, "input.txt")
    val outputDest: File = new File(td, "output.txt")
    val expectedOutput = mkExpectedOutput(replacementMap)(input)

    inputDest.exists() shouldEqual false
    outputDest.exists() shouldEqual false

    val r = for {
      _ <- writeToFile(input, inputDest)
      _ <- CheckFile.composeAll(inputFileChecks)(inputDest)
      res <- sqlpp.Run.applyEither(mkArgs(inputDest, outputDest))
      _ <- CheckFile.composeAll(outputFileChecks)(outputDest)
      _ <- checkOutput(outputDest, expectedOutput)
    } yield {
      res
    }

    r should be ('right)
    //TODO: check exit code
  }
}

object BasicIntegrationTest {
  class BasicIntegrationTestError(override val msg: String)
    extends TestError(msg)

  class UnexpectedOutputError(val actual: String, val expected: String)
    extends TestError(
      UnexpectedOutputError.formatMsg(
        "BasicIntegrationTests")(actual, expected))


  object UnexpectedOutputError {
    private val leftCharacter: String = ">"

    private def formatLines(fmt: Formatter)(x: String): Unit = {
      if(x.isEmpty) {
        fmt.format("<blank string>\n")
      } else if(!x.isEmpty && x.trim.isEmpty) {
        fmt.format(s"<string consists of ${x.length} whitespace characters>\n")
      } else {
        import scala.collection.JavaConverters
        x.lines.toArray.foreach { line =>
          fmt.format("%s\t%s\n", leftCharacter, line)
        }
      }
    }

    /**
     * Better error message formatting
     * Otherwise the messages given can be very long on a small number of lines--
     * difficult for humans to parse
     * @param testName
     * @param actual
     * @param expected
     * @return
     */
    private def formatMsg(testName: String)
                         (actual: String, expected: String): String = {
      val fmt: Formatter = {
        val sb: StringBuffer = new StringBuffer()
        new Formatter(sb)
      }

      fmt.format("Unexpected output in %s\n", testName)

      fmt.format("Expected output:\n")
      formatLines(fmt)(expected)

      fmt.format("------------------\n")

      fmt.format("Actual output:\n")
      formatLines(fmt)(actual)


      fmt.toString
    }
  }
}
