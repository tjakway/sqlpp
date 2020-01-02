package com.jakway.sqlpp.config.test.template

import java.util.Formatter

import com.jakway.sqlpp.config.test.template.TemplateEngineTestAsserter.DocumentMatcher
import com.jakway.sqlpp.config.test.util.StringCmpFunction
import com.jakway.sqlpp.error.SqlppError
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.collection.immutable.StringOps

trait TemplateEngineTestAsserter { this: Matchers =>
  type TemplateOutput = String

  def stringCmpFunction: StringCmpFunction = StringCmpFunction.StrictEquality

  protected def assertTemplateEngineTest(
                    actualRes: String,
                    expectedRes: String): Assertion = {

    assertTemplateEngineTest(Right(actualRes), Right(expectedRes))
  }

  protected def assertTemplateEngineTest(
                    actualRes: Either[SqlppError, TemplateOutput],
                    expectedRes: Either[SqlppError, TemplateOutput]): Assertion = {

    val res = for {
      expected <- expectedRes
      actual <- actualRes
    } yield { (expected, actual) }

    res should be ('right)
    val (expected, actual) = res.right.get

    actual.length should be >0
    actual should matchExpectedTemplateOutput(expected)
  }

  protected def matchExpectedTemplateOutput(expected: TemplateOutput) =
    new DocumentMatcher(expected, stringCmpFunction)
}

object TemplateEngineTestAsserter {
  class DocumentMatcher(val expected: String,
                        val stringCmpFunction: StringCmpFunction)
    extends Matcher[String] {

    protected def formatDocument(str: String,
                                 indent: String = "  "): String = {
      //tab-indent
      val fmt: Formatter = {
        val sb = new StringBuffer()
        new Formatter(sb)
      }
      (str: StringOps).lines.foreach { line =>
        fmt.format("%s%s\n", indent, line)
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

      MatchResult(
        stringCmpFunction(expectedDoc, actualDoc),
        failureMsg, successMsg)
    }
  }
}
