package com.jakway.sqlpp.config.test.template

import com.jakway.sqlpp.config.test.testconfig.TestConfig.ReadTemplateEngineTestOptions
import com.jakway.sqlpp.config.test.template.ParseTest.Errors.{AttributeError, ElementError, ParseAttributeContentsError, UnexpectedElementError}
import com.jakway.sqlpp.config.test.template.TemplateEngineTestSet.{BackendName, BackendResult, RequiredBackendError}
import com.jakway.sqlpp.config.test.util.LogEither
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.backend.Backend
import com.jakway.sqlpp.util.TryToEither
import javax.xml.parsers.DocumentBuilderFactory
import org.slf4j.{Logger, LoggerFactory}
import org.w3c.dom.{Document, Element, Node, NodeList}

import scala.annotation.tailrec
import scala.collection.immutable.StringOps
import scala.util.Try
import scala.xml.InputSource


case class TemplateEngineTestSetWithBackends(settings: TemplateEngineTestSetWithBackends.Settings,
                                             input: String,
                                             expectedResults: Map[Backend, BackendResult])

object TemplateEngineTestSetWithBackends {
  type Settings = TemplateEngineTestSet.Settings
}


class TemplateEngineTestSet(val settings: TemplateEngineTestSet.Settings)
                           (val input: String,
                            val expectedResults: Map[BackendName, BackendResult]) {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def associateBackends(backends: Set[Backend],
                        requireAtLeastOneBackend: Boolean = true,
                        requireAllBackends: Boolean = false):
    Either[SqlppError, TemplateEngineTestSetWithBackends] = {

    val empty: Either[SqlppError, Map[Backend, BackendResult]] =
      Right(Map.empty)

    val res = expectedResults.foldLeft(empty) {
      case (eAcc, (thisBackendName, thisBackendExpectedResult)) =>
        eAcc.flatMap { acc =>

          Backend.Lookup.findMatchWithoutOverlap(backends)(thisBackendName)
            .map {
              case Some(foundBackend) =>
                acc.updated(foundBackend, thisBackendExpectedResult)
              case None => {
                logger.warn(
                  "Could not find backend for test result " +
                    s"$thisBackendName -> $thisBackendExpectedResult")
                acc
              }
            }
        }
      }


    def errMsg(m: String): String =
      m + s" in call to associateBackends(backends = $backends)" +
        s" with expectedResults = $expectedResults"

    lazy val needAtLeastOneBackendError =
      new RequiredBackendError(errMsg(s"Expected at least 1 backend"))

    lazy val needAllBackendsError =
      new RequiredBackendError(errMsg(s"Expected to find a backend for" +
        s" every test case (${expectedResults.size} backends)"))

    val associatedBackendsRes = res
      .filterOrElse(_.size > 0, needAtLeastOneBackendError)
      .filterOrElse(_.size == expectedResults.size, needAllBackendsError)
      .map { foundBackends =>
        TemplateEngineTestSetWithBackends(settings,
          input, foundBackends)
      }

    LogEither(logger,
      associatedBackendsRes, "Associate backends result: %s")

    associatedBackendsRes
  }
}

object TemplateEngineTestSet {
  type BackendName = String
  type BackendResult = String

  class Settings(val normalized: Boolean,
                 val allowEmptyTests: Boolean)

  object Settings {
    val defaultNormalizeTestWhitespaceAttribute: Boolean = false
    val defaultAllowEmptyTests: Boolean = false

    val default: Settings =
      new Settings(
        defaultNormalizeTestWhitespaceAttribute,
        defaultAllowEmptyTests)
  }

  class TemplateEngineTestSetError(override val msg: String)
    extends SqlppError(msg)

  class RequiredBackendError(override val msg: String)
    extends TemplateEngineTestSetError(msg)
}

object ParseTest {
  //import default fields
  import TemplateEngineTestSet.Settings._

  val logger: Logger = LoggerFactory.getLogger(getClass)

  val newlineReplacement: String = " "

  object Names {
    val rootNode: String = "test"
    val normalizeTestWhitespace: String = "normalizeTestWhitespace"
    val allowEmptyTests: String = "allowEmptyTests"
    val testInputNode: String = "input"
    val testResultElement: String = "result"
    val backendNameAttribute: String = "backend"
  }

  private def normalize(str: String): String = {

    def normalizeLines(s: String): String = {
      val zero: java.lang.StringBuilder = new java.lang.StringBuilder()

      //see https://stackoverflow.com/questions/52815574/scala-12-x-and-java-11-string-lines-how-to-force-the-implicit-conversion-in-a/52815819#52815819
      (s: StringOps).lines
        .foreach { thisLine =>

          zero.append(thisLine)
        }
      zero.toString
    }

    def normalizeWhitespace(s: String): String =
      s.replaceAll("""\s+""", " ")

    normalizeWhitespace(normalizeLines(str))
  }

  private def normalize(test: TemplateEngineTestSet): TemplateEngineTestSet = {
    val normalizedResults = test.expectedResults.mapValues(normalize)

    new TemplateEngineTestSet(test.settings)(
      normalize(test.input), normalizedResults)
  }

  private def normalize(test: TemplateEngineTestSet, flag: Boolean): TemplateEngineTestSet = {
    if(flag) {
      normalize(test)
    } else {
      test
    }
  }

  object Errors {
    class ParseTestError(override val msg: String)
      extends SqlppError(msg)

    class AttributeError(override val msg: String)
      extends ParseTestError(msg)

    class ParseAttributeContentsError(val attrName: String,
                                      val attrValue: String,
                                      val throwable: Throwable)
      extends AttributeError(
        s"Error parsing attribute $attrName" +
          s" with contents $attrValue: " +
          SqlppError.formatThrowable(throwable))

    class ElementError(override val msg: String)
      extends ParseTestError(msg)

    class UnexpectedElementError(val expected: String,
                                 val actual: Element)
      extends ElementError(s"Expected $expected but got $actual")
  }

  private object ParseDocument {

    object ParseAttributeContents {
      /**
       * for string-valued attributes
       * @return
       */
      def stringF: String => Either[SqlppError, String] =
        x => Right(x)

      def booleanF(attrName: String): String => Either[SqlppError, Boolean] = {
        x => TryToEither(
          new ParseAttributeContentsError(attrName, x, _))(
          Try(x.toBoolean))
      }
    }


    private def parseAttribute[A](elem: Element,
                                  attrName: String,
                                  defaultValue: Option[A],
                                  parseF: String => Either[SqlppError, A]):
      Either[SqlppError, A] = {

      lazy val emptyError =
        Left(new AttributeError(s"Attribute with name $attrName is empty"))

      if(elem.hasAttribute(attrName)) {
        val attrValue = elem.getAttribute(attrName).trim

        if(attrValue.nonEmpty) {
          parseF(attrValue)
        } else {
          emptyError
        }
      } else {
        defaultValue match {
          case Some(x) => Right(x)
          case None => emptyError
        }
      }
    }

    private def parseNormalizeTestWhitespaceAttribute(root: Element):
      Either[SqlppError, Boolean] = {

      val attrName = Names.normalizeTestWhitespace

      parseAttribute[Boolean](root,
        attrName,
        Some(defaultNormalizeTestWhitespaceAttribute),
        ParseAttributeContents.booleanF(attrName))
    }

    private def parseAllowEmptyTests(root: Element):
      Either[SqlppError, Boolean] = {

      val attrName = Names.allowEmptyTests

      parseAttribute[Boolean](root,
        attrName,
        Some(defaultAllowEmptyTests),
        ParseAttributeContents.booleanF(attrName))
    }

    def parseSettings(root: Element): Either[SqlppError, TemplateEngineTestSet.Settings] = {
      for {
        normalize <- parseNormalizeTestWhitespaceAttribute(root)
        allowEmptyTests <- parseAllowEmptyTests(root)
      } yield {
        new TemplateEngineTestSet.Settings(normalize, allowEmptyTests)
      }
    }

    object Queries {
      private def nodeListToSeq(nl: NodeList): Seq[Node] = {
        @tailrec
        def helper(acc: Seq[Node], index: Int): Seq[Node] = {
          if(index == nl.getLength) {
            acc
          } else {
            helper(acc :+ nl.item(index), index + 1)
          }
        }

        helper(Seq.empty, 0)
      }

      def expectName(name: String, element: Element): Either[SqlppError, Unit] = {
        if(element.getTagName == name) {
          Right({})
        } else {
          Left(new UnexpectedElementError(name, element))
        }
      }

      def getChildrenWithName(name: String, element: Element): Seq[Element] = {
        nodeListToSeq(element.getElementsByTagName(name))
          .filter(_.isInstanceOf[Element])
          .map(x => x.asInstanceOf[Element])
      }

      def getAttribute(onError: => SqlppError)
                      (name: String, e: Element): Either[SqlppError, String] = {
        if(e.hasAttribute(name)) {
          Option(e.getAttribute(name)).filter(_.trim.nonEmpty) match {
            case Some(x) => Right(x.trim)
            case None => Left(onError)
          }
        } else {
          Left(onError)
        }
      }

      def getNodeValue(onError: => SqlppError)
                      (e: Element,
                       allowEmpty: Boolean = defaultAllowEmptyTests):
        Either[SqlppError, String] = {

        def optFilter: Option[String] => Option[String] = { x =>
          if(allowEmpty) {
            x
          } else {
            x.filter(_.trim.nonEmpty)
          }
        }

        optFilter(Option(e.getTextContent)) match {
          case Some(x) => Right(x.trim)
          case None => Left(onError)
        }
      }
    }
    import Queries._



    def parseResultNode(resultNode: Element,
                        allowEmpty: Boolean = defaultAllowEmptyTests):
      Either[SqlppError, (String, String)] = {

      lazy val backendNameAttrError =
        new AttributeError(s"Expected result node to have a" +
          " \"" + Names.backendNameAttribute + "\" attribute containing" +
          "the name of the backend")

      lazy val noTestResultError =
        new ElementError(s"Expected non-empty test results for $resultNode")

      for {
        _ <- expectName(Names.testResultElement, resultNode)
        backendName <- getAttribute(backendNameAttrError)(Names.backendNameAttribute, resultNode)
        testResult <- getNodeValue(noTestResultError)(resultNode, allowEmpty)
      } yield {
        (backendName, testResult)
      }
    }

    def parseInputNode(inputNode: Element,
                       allowEmpty: Boolean):
      Either[SqlppError, String] = {

      val noTestInput = new ElementError(s"Expected ${Names.testInputNode} " +
        s"$inputNode to contain the test input")
      for {
        _ <- expectName(Names.testInputNode, inputNode)
        res <- getNodeValue(noTestInput)(inputNode, allowEmpty)
      } yield {
        res
      }
    }

    def parseInputNode(inputNodes: Seq[Element],
                       allowEmpty: Boolean): Either[SqlppError, String] = {
      if(inputNodes.length == 1) {
        parseInputNode(inputNodes.head, allowEmpty)
      } else {
        Left(new ElementError(s"Expected there to be only 1 " +
          s"${Names.testInputNode} defining the test case"))
      }
    }

    def parseResultNodes(resultNodes: Seq[Element],
                         allowEmpty: Boolean = defaultAllowEmptyTests):
      Either[SqlppError, Seq[(String, String)]] = {

      val empty: Either[SqlppError, Seq[(String, String)]] = Right(Seq.empty)
      resultNodes.foldLeft(empty) {
        case (eAcc, thisNode) => eAcc.flatMap { acc =>
          parseResultNode(thisNode, allowEmpty).map(res => acc :+ res)
        }
      }
    }

    def parseRootElement(root: Element): Either[SqlppError, TemplateEngineTestSet] = {
      for {
        settings <- parseSettings(root)
        _ <- expectName(Names.rootNode, root)
        rawInputNodes = getChildrenWithName(Names.testInputNode, root)
        testInput <- parseInputNode(rawInputNodes, settings.allowEmptyTests)
        rawResultNodes = getChildrenWithName(Names.testResultElement, root)
        resultNodeValues <- parseResultNodes(
          rawResultNodes,
          settings.allowEmptyTests)
      } yield {
        new TemplateEngineTestSet(settings)(testInput, resultNodeValues.toMap)
      }
    }

    def apply(document: Document):
      Either[SqlppError, TemplateEngineTestSet] = {
      parseRootElement(document.getDocumentElement)
    }
  }


  /**
   * master function
   * @param testLocation
   * @return
   */
  def readTest(testLocation: InputSource): Either[SqlppError, TemplateEngineTestSet] = {
    val documentBuilderFactory = DocumentBuilderFactory.newInstance()
    val documentBuilder = documentBuilderFactory.newDocumentBuilder()
    val document = documentBuilder.parse(testLocation)

    val res = ParseDocument(document)
      //handle normalizeTestResults flag
      .map { res =>
        normalize(res, res.settings.normalized)
      }

    lazy val msg: String = "ParseTest result: " + res
    //increase level on failure
    res match {
      case Right(_) => logger.debug(msg)
      case Left(_) => logger.error(msg)
    }

    res
  }

  def readTest(testLocation: InputSource,
               backends: Set[Backend],
               requireAtLeastOneBackend: Boolean = true,
               requireAllBackends: Boolean = false):
    Either[SqlppError, TemplateEngineTestSetWithBackends] = {

    readTest(testLocation).flatMap { tests =>
      tests.associateBackends(
        backends, requireAtLeastOneBackend, requireAllBackends)
    }
  }

  def readTest(testLocation: InputSource,
               backends: Set[Backend],
               readTemplateEngineTestOptions: ReadTemplateEngineTestOptions):
    Either[SqlppError, TemplateEngineTestSetWithBackends] =
    readTest(
      testLocation,
      backends,
      readTemplateEngineTestOptions.requireAtLeastOneBackend,
      readTemplateEngineTestOptions.requireAllBackends)
}
