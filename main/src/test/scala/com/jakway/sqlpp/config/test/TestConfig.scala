package com.jakway.sqlpp.config.test

import java.nio.charset.StandardCharsets

trait TestConfig {
  val encodings: Set[String] = Set(StandardCharsets.UTF_8.displayName())
}

object TestConfig extends TestConfig
