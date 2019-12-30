package com.jakway.sqlpp.config.test.util

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64

import com.jakway.sqlpp.config.test.TestConfig
import com.jakway.sqlpp.config.test.error.TestError
import com.jakway.sqlpp.config.test.util.TemplateTestUtil.HashTemplateSourceError
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.TemplateEngine
import com.jakway.sqlpp.util.TryToEither

import scala.util.Try

trait TemplateTestUtil {
  val hashName: String = "SHA-256"
  val keyPrefix: String = "template_source_key_"

  def getTemplateEngine: TestConfig => Either[SqlppError, TemplateEngine] = {
    testConfig =>
      TemplateEngine.apply(
        testConfig.encoding)(
        testConfig.loaderTypes)(
        testConfig.extraTemplateOptions)(
        testConfig.additionalVelocityProperties)
  }

  def getTemplateSourceHash(templateString: String): Either[SqlppError, String] = {
    TryToEither(new HashTemplateSourceError(_)) {
      Try {
        val digest = MessageDigest.getInstance(hashName)

        //encoding doesn't matter here so long as we always use the same one
        val hash = digest.digest(
          templateString.getBytes(StandardCharsets.UTF_8.displayName()))

        Base64.getEncoder.encodeToString(hash)
      }
    }
  }

  def getTemplateSourceKey(templateString: String): Either[SqlppError, String] =
    getTemplateSourceHash(templateString)
      .map(hash => keyPrefix + hash)
}

object TemplateTestUtil {
  class HashTemplateSourceError(val throwable: Throwable)
    extends TestError(SqlppError.formatThrowableCause(throwable))
}
