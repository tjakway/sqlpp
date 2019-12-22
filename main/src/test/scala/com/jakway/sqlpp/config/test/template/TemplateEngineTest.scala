package com.jakway.sqlpp.config.test.template

import com.jakway.sqlpp.config.test.WithDefaultTestConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

abstract class TemplateEngineTest(val testResource: String,
                                  val testName: String = getClass.getName)
  extends AnyFlatSpec
    with Matchers
    with TemplateEngineTestAsserter
    with WithDefaultTestConfig {

  testName should "pass template engine tests" in {

  }
}
