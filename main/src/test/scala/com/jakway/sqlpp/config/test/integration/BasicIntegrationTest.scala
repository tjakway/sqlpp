package com.jakway.sqlpp.config.test.integration

import com.jakway.sqlpp.config.test.framework.WithTempDirBeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.{Logger, LoggerFactory}

class BasicIntegrationTest
  extends AnyFlatSpec
    with Matchers
    with WithTempDirBeforeAndAfter {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  val args = Seq(
    "--no-create-profile-dir",
    "--resource-loader-types", "file,class",

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
