package com.jakway.sqlpp.config.test

import java.io.InputStream
import java.nio.charset.StandardCharsets

import com.jakway.sqlpp.config.Defaults
import com.jakway.sqlpp.config.checked.Config
import com.jakway.sqlpp.config.output.OutputTarget
import com.jakway.sqlpp.config.test.TestConfig.ReadTemplateEngineTestOptions
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.backend.Backend
import org.scalacheck.Gen

object GenTestConfig {
  val allPossibleEncodings: Set[String] = Set(StandardCharsets.UTF_8.displayName())

  /**
   * TODO: find somewhere more appropriate for this
   */
  val genEncoding: Gen[String] = Gen.oneOf(allPossibleEncodings)

}

trait HasTestBackends {
  def getTestBackends: Either[SqlppError, Set[Backend]]
}

trait TestConfig extends HasTestBackends {
  val config: Config
  val readTemplateEngineTestOptions: ReadTemplateEngineTestOptions
}

object TestConfig {
  //*** replace with actual input stream in each test ***
  val defaultSource: InputStream = null
  val defaultOutputTargets: Seq[OutputTarget] = Seq.empty

  val default: TestConfig = new TestConfig {
    override val config: Config =
      Defaults.Config.default(defaultSource, defaultOutputTargets)

    override def getTestBackends: Either[SqlppError, Set[Backend]] = {
      getDefaultTestBackends
    }

    override val readTemplateEngineTestOptions: ReadTemplateEngineTestOptions =
      ReadTemplateEngineTestOptions.default
  }

  //TODO
  val getDefaultTestBackends: Either[SqlppError, Set[Backend]] = Right(Set())

  class ReadTemplateEngineTestOptions(val requireAtLeastOneBackend: Boolean,
                                      val requireAllBackends: Boolean)

  object ReadTemplateEngineTestOptions {
    val defaultRequireAtLeastOneBackend: Boolean = true
    val defaultRequireAllBackends: Boolean = false

    val default: ReadTemplateEngineTestOptions =
      new ReadTemplateEngineTestOptions(
        defaultRequireAtLeastOneBackend,
        defaultRequireAllBackends)
  }
}

trait HasTestConfig {
  val testConfig: TestConfig
}

trait WithDefaultTestConfig extends HasTestConfig {
  override val testConfig: TestConfig = TestConfig.default
}
