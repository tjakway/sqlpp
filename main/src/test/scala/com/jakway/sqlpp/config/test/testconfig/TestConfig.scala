package com.jakway.sqlpp.config.test.testconfig

import java.nio.charset.StandardCharsets

import com.jakway.sqlpp.config.Defaults
import com.jakway.sqlpp.config.checked.TemplateStringInfo
import com.jakway.sqlpp.config.test.TestResources
import com.jakway.sqlpp.config.test.testconfig.TestConfig.ReadTemplateEngineTestOptions
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.GeneralVelocityOptions
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.{ExtraTemplateOptions, PropertyMap}
import com.jakway.sqlpp.template.backend.{Backend, PropertiesResourceBackend}
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
  val readTemplateEngineTestOptions: ReadTemplateEngineTestOptions

  val encoding: String
  val templateStringInfo: TemplateStringInfo

  val loaderTypes: Set[LoaderType]
  val extraTemplateOptions: ExtraTemplateOptions
  val additionalVelocityProperties: PropertyMap
}

object TestConfig {
  val default: TestConfig = new TestConfig {
    override def getTestBackends: Either[SqlppError, Set[Backend]] = {
      getDefaultTestBackends
    }

    override val readTemplateEngineTestOptions: ReadTemplateEngineTestOptions =
      ReadTemplateEngineTestOptions.default

    override val encoding: String = Defaults.defaultEncoding.displayName()
    override val templateStringInfo: TemplateStringInfo =
      Defaults.TemplateStringInfo.default

    override val loaderTypes: Set[LoaderType] =
      Defaults.Config.defaultResourceLoaderTypes

    override val extraTemplateOptions: ExtraTemplateOptions = Defaults.extraTemplateOptions

    override val additionalVelocityProperties: PropertyMap = GeneralVelocityOptions(encoding, verbose = false)
  }

  def getDefaultTestBackends: Either[SqlppError, Set[Backend]] = Right {
    Set(
      new PropertiesResourceBackend(
        Set("defaults"), TestResources.defaultsBackend),
      new PropertiesResourceBackend(
        Set("h2"), TestResources.h2Backend),
      new PropertiesResourceBackend(
        Set("postgres"), TestResources.postgresBackend)
    )
  }

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
