package com.jakway.sqlpp.config.test

import java.nio.charset.StandardCharsets

import org.scalacheck.Gen

trait TestConfig {
  val encodings: Set[String] = Set(StandardCharsets.UTF_8.displayName())

  val genEncoding: Gen[String] = Gen.oneOf(encodings)
}

object TestConfig extends TestConfig
