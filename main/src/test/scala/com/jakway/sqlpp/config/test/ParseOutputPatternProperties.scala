package com.jakway.sqlpp.config.test

import org.scalatest.Assertion
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers

class ParseOutputPatternProperties
  extends AnyPropSpec
    with Matchers {
}

object ParseOutputPatternProperties {
  class ParseOutputPatternTest(val encoding: String,
                               val requireFormatSymbol: Boolean,
                               val toParse: String)
}
