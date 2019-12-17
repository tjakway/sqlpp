package com.jakway.sqlpp.config.test.template

import java.util.Formatter

import com.jakway.sqlpp.config.test.template.TemplateEngineTest.DocumentMatcher
import com.jakway.sqlpp.error.SqlppError
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.matchers.should.Matchers

abstract class TemplateEngineTest(val testName: String = getClass.getName)
  extends AnyFlatSpec with Matchers {
  type TemplateOutput = String

  val testActionName: String = TemplateEngineTest.defaultTestActionName

  def getExpected: Either[SqlppError, TemplateOutput]
  def getActual: Either[SqlppError, TemplateOutput]

  testName should testActionName in {
    val res = for {
      expected <- getExpected
      actual <- getActual
    } yield { (expected, actual) }

    res should be ('right)
    val (expected, actual) = res.right.get
    actual should matchExpectedTemplateOutput(expected)
  }

  protected def matchExpectedTemplateOutput(expected: TemplateOutput) =
    new DocumentMatcher(expected)

  protected def matches(expected: TemplateOutput,
                        actual: TemplateOutput): MatchResult = {
    new DocumentMatcher(expected).apply(actual)
  }
}

object TemplateEngineTest {
  val defaultTestActionName: String = "match expected output"

  class DocumentMatcher(val expected: String)
    extends Matcher[String] {

    protected def formatDocument(str: String): String = {
      //tab-indent
      val fmt: Formatter = {
        val sb = new StringBuffer()
        new Formatter(sb)
      }
      str.lines.foreach { line =>
        fmt.format("\t%s\n", line)
      }

      fmt.toString
    }

    protected def resultMsg(result: String,
                            expectedDoc: String,
                            actualDoc: String): String = {
      val fmt = {
        val sb = new StringBuffer()
        new Formatter(sb)
      }

      fmt.format("Expected %s actual\n", result)
      fmt.format("Expected:\n")
      fmt.format("%s\n", expectedDoc)
      fmt.format("Actual:\n")
      fmt.format("%s\n", actualDoc)

      fmt.toString
    }

    override def apply(actual: String): MatchResult = {
      val expectedDoc: String = formatDocument(expected)
      val actualDoc: String = formatDocument(actual)

      val successMsg = resultMsg("matched", expectedDoc, actualDoc)
      val failureMsg = resultMsg("did not match", expectedDoc, actualDoc)

      MatchResult(expectedDoc == actualDoc, failureMsg, successMsg)
    }
  }
}
