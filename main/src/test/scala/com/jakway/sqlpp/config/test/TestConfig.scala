package com.jakway.sqlpp.config.test

import java.nio.charset.StandardCharsets

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.backend.Backend
import org.scalacheck.Gen

trait HasTestBackends {
  def getTestBackends: Either[SqlppError, Set[Backend]]
}

trait TestConfig extends HasTestBackends {
  val encodings: Set[String] = Set(StandardCharsets.UTF_8.displayName())

  /**
   * TODO: find somewhere more appropriate for this
   */
  val genEncoding: Gen[String] = Gen.oneOf(encodings)
}

object TestConfig {
  val default: TestConfig = new TestConfig {
    override def getTestBackends: Either[SqlppError, Set[Backend]] = {
      getDefaultTestBackends
    }
  }

  //TODO
  val getDefaultTestBackends: Either[SqlppError, Set[Backend]] = Right(Set())
}

trait HasTestConfig {
  val testConfig: TestConfig
}

trait WithDefaultTestConfig extends HasTestConfig {
  override val testConfig: TestConfig = TestConfig
}
