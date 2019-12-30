package com.jakway.sqlpp.config.test.template

import java.io.{ByteArrayInputStream, InputStream, StringWriter, Writer}
import java.nio.charset.StandardCharsets

import com.jakway.sqlpp.config.test.WithDefaultTestConfig
import com.jakway.sqlpp.config.test.util.{TemplateTestUtil, TestUtil}
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.{TemplateEngine, ValueSource}
import com.jakway.sqlpp.template.backend.Backend
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.xml.InputSource

abstract class TemplateEngineTest(val testResource: String,
                                  val testName: String = getClass.getName)
  extends AnyFlatSpec
    with Matchers
    with TemplateEngineTestAsserter
    with TemplateTestUtil
    with WithDefaultTestConfig {

  protected val loaderTypes: Set[LoaderType] = {
    import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders

    Set(
      StandardResourceLoaders.ClassLoader,
      StandardResourceLoaders.StringLoader
    )
  }

  protected def readTest: Either[SqlppError, TemplateEngineTestSetWithBackends] = {
    for {
      testBackends <- testConfig.getTestBackends
      testResourceInputSource <-
        TestUtil.openTestResource(testResource).map(new InputSource(_))

      test <- ParseTest.readTest(
        testResourceInputSource,
        testBackends,
        testConfig.readTemplateEngineTestOptions)
    } yield {
      test
    }
  }

  private def getTestInput(forTest: TemplateEngineTestSetWithBackends):
    (String, Map[Backend, StringWriter]) = {

    val stringWriters: Map[Backend, StringWriter] =
      openStringWriters(forTest.expectedResults)

    (forTest.input, stringWriters)
  }

  private def openStringWriters[A, B](forMap: Map[A, B],
                                      initialSize: Int =
                                         TemplateEngineTest
                                           .defaultStringWriterInitialSize):
    Map[A, StringWriter] = {

    forMap.mapValues(x => new StringWriter(initialSize))
  }

  protected def runTests():
     Either[SqlppError, Unit] = {

    for {
      tests <- readTest
      (input, swMap) = getTestInput(tests)

      //TODO: handle other charsets
      templateAsInputStream: InputStream =
        new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

      templateEngine <- getTemplateEngine(testConfig)

      templateSourceKey <- getTemplateSourceKey(input)

      template <- templateEngine
        .loadTemplateFromInputStream(
          templateAsInputStream)(
          templateSourceKey)(
          testConfig.config.templateStringInfo.stringResourceRepositoryName)(
          testConfig.config.inputEncoding)

      //need to upcast the Map[Backend, StringWriter] to Map[ValueSource, Writer]
      ioMap = swMap.map {
        case (backend, sw) => (backend: ValueSource, sw: Writer)
      }

      _ <- templateEngine.multiApplyWriters(
        template,
        ioMap)
    } yield {
      //TODO: check results
      {}
    }


    //TODO
    ???
  }

  testName should "pass template engine tests" in {
  }
}

object TemplateEngineTest {
  val defaultStringWriterInitialSize: Int = 4096
}
