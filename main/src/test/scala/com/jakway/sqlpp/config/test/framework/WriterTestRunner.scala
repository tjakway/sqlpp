package com.jakway.sqlpp.config.test.framework

import java.io.StringWriter
import java.util.Formatter

import com.jakway.sqlpp.config.checked.Config
import com.jakway.sqlpp.config.test.error.TestError
import com.jakway.sqlpp.config.test.framework.WriterTestRunner.{WriterTestCheckIOMapError, WriterTestRunnerError}
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.ValueSource
import com.jakway.sqlpp.util.TryClose
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers

trait WriterTestRunner { this: Matchers =>
  protected def closeWriters: Boolean = WriterTestRunner.defaultCloseWriters

  private def withNewIOMap(config: Config): Config = {
    def newIOMap = config.ioMap.map {
      case (vs, writer) => {
        if(closeWriters) {
          TryClose(writer, Some(s"Writer for $vs"))
        }

        (vs, new StringWriter())
      }
    }

    config.copy(ioMap = newIOMap)
  }

  protected def runWriterTest: Config =>
    Map[ValueSource, String] =>
    (Config => Either[SqlppError, Map[ValueSource, String]]) =>
    Assertion = {
    (c: Config) =>
      (expected: Map[ValueSource, String]) =>
      (testF: (Config => Either[SqlppError, Map[ValueSource, String]])) => {
        val newConfig = withNewIOMap(c)
        val newIOMap = newConfig.ioMap

        val testRes = testF(newConfig)

        val zero: Either[SqlppError, Map[ValueSource, String]] =
          Right(Map.empty)
        val eActual = newIOMap.foldLeft(zero) {
          case (eAcc, (vs, writer)) => eAcc.flatMap { acc =>

            if(writer.isInstanceOf[StringWriter]) {
              Right(acc.updated(vs, writer.toString))
            } else {
              Left(new WriterTestRunnerError(
                s"Writer for $vs was not an instance of StringWriter"))
            }
          }
        }

        val res = for {
          actual <- eActual
          _ <- checkIOMap(actual, expected)
        } yield {}

        res should be ('right)
      }
  }

  protected def defaultStringsMatchF(left: String, right: String): Boolean =
    left == right

  protected def stringsMatchF: (String, String) => Boolean =
    defaultStringsMatchF

  protected def checkIOMap(actual: Map[ValueSource, String],
                           expected: Map[ValueSource, String]):
    Either[SqlppError, Unit] = {

    def newFmt: Formatter = {
      val sb = new StringBuffer()
      new Formatter(sb)
    }

    def printErrMsg(vs: ValueSource,
                    actual: String,
                    optExpected: Option[String]): String = {
      val fmt: Formatter = newFmt

      optExpected match {
        case Some(expected) => {
          fmt.format("Actual < %s > didn't match " +
            "expected < %s > for ValueSource %s",
            actual, expected, vs)
        }
        case None => {
          fmt.format("Couldn't find expected string for ValueSource %s " +
            "with actual result < %s >", vs, actual)
        }
      }

      fmt.toString
    }

    val zero: Seq[(ValueSource, String)] = Seq.empty
    val accErrors = actual.foldLeft(zero) {
      case (acc, (vs, actualS)) => {
        expected.get(vs) match {
            //success, don't accumulate any errors
          case Some(foundExpected) =>  {
            if(stringsMatchF(actualS, foundExpected)) {
              acc
            } else {
              acc :+ (vs,
                printErrMsg(vs, actualS, Some(foundExpected)))
            }
          }

          case None => acc :+ (vs, printErrMsg(vs, actualS, None))
        }
      }
    }

    //TODO (low priority):
    // probably left-align ValueSource and right-align error message
    val res: Seq[String] = accErrors.map {
      case (vs, err) => s"$vs -> $err"
    }

    def sizeMatchCheck: Seq[String] = {
      if(actual.size == expected.size) {
        Seq.empty
      } else {
        Seq(s"Expected actual (${actual.size})" +
          s" to have the same size as expected (${expected.size})")
      }
    }

    val allErrs: Seq[String] = sizeMatchCheck ++ res
    if(allErrs.isEmpty) {
      Right({})
    } else {
      val fmt = newFmt

      allErrs.foreach { thisErr =>
        fmt.format("%s\n", thisErr)
      }

      Left(new WriterTestCheckIOMapError(fmt.toString))
    }
  }
}

object WriterTestRunner {
  class WriterTestRunnerError(override val msg: String)
    extends TestError(msg)

  class WriterTestCheckIOMapError(override val msg: String)
    extends WriterTestRunnerError(msg)

  val defaultCloseWriters: Boolean = true
}
