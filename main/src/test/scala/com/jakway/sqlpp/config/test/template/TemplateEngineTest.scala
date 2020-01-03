package com.jakway.sqlpp.config.test.template

import java.io.{ByteArrayInputStream, InputStream, StringWriter, Writer}
import java.nio.charset.StandardCharsets

import com.jakway.sqlpp.config.test.WithDefaultTestConfig
import com.jakway.sqlpp.config.test.error.TestError
import com.jakway.sqlpp.config.test.template.TemplateEngineTest.CannotFindBackendResultError
import com.jakway.sqlpp.config.test.template.TemplateEngineTestSet.BackendResult
import com.jakway.sqlpp.config.test.util.{TemplateTestUtil, TestUtil}
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.ValueSource
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

  protected def printTestSubject: String = testName
  protected def printBackendTestAction(backend: Backend): String =
    s"$backend should pass backend test"


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

  private def checkTestOutput(writers: Map[Backend, StringWriter],
                              expectedResults: Map[Backend, BackendResult]): Unit = {

    //get results from the writers
    val results = writers.mapValues(_.toString)

    results should have size expectedResults.size

    expectedResults.foreach {
      case (thisBackend, thisExpectedResult) => {

        //new test
        printTestSubject should printBackendTestAction(thisBackend) in {
          val actual = writers.get(thisBackend) match {
            case Some(x) => Right(x.toString)
            case None => Left(
              new CannotFindBackendResultError(
                s"Could not find template output for backend $thisBackend"))
          }

          assertTemplateEngineTest(actual, Right(thisExpectedResult))
        }
      }
    }

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
          testConfig.templateStringInfo.stringResourceRepositoryName)(
          testConfig.encoding)

      //need to upcast the Map[Backend, StringWriter] to Map[ValueSource, Writer]
      ioMap = swMap.map {
        case (backend, sw) => (backend: ValueSource, sw: Writer)
      }

      _ <- templateEngine.multiApplyWriters(
        template,
        ioMap)
    } yield {
      checkTestOutput(swMap, tests.expectedResults)
    }
  }

  runTests() should be ('right)
}

object TemplateEngineTest {
  val defaultStringWriterInitialSize: Int = 4096

  class TemplateEngineTestError(override val msg: String)
    extends TestError(msg)

  class CannotFindBackendResultError(override val msg: String)
    extends TemplateEngineTestError(msg)

}
