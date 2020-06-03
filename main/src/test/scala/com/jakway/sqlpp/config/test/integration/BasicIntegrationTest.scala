package com.jakway.sqlpp.config.test.integration

import java.io.File

import com.jakway.sqlpp.config.test.framework.{TestFileUtils, WithTempDirBeforeAndAfter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.{Logger, LoggerFactory}

class BasicIntegrationTest
  extends AnyFlatSpec
    with Matchers
    with WithTempDirBeforeAndAfter
    with TestFileUtils{
  val logger: Logger = LoggerFactory.getLogger(getClass)

  def args(input: File, outputDest: File) = Seq(
    "--no-create-profile-dir",
    "--resource-loader-types", "file,class",
    "--source", input,
    "--output", outputDest.getAbsolutePath
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



  "Sqlpp" should "run" in {

  }

}

object BasicIntegrationTest {

}
