package com.jakway.sqlpp.config.test.template

import com.jakway.sqlpp.config.test.template.ParseTest.Errors.{AttributeError, ElementError, UnexpectedElementError}
import com.jakway.sqlpp.config.test.template.TemplateEngineTestSet.{BackendName, BackendResult}
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.Backend
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.{Document, Element, Node, NodeList}

import scala.annotation.tailrec
import scala.util.Try
import scala.xml.InputSource


class TemplateEngineTestSet(val settings: TemplateEngineTestSet.Settings)
                           (val input: String,
                            val expectedResults: Map[BackendName, BackendResult])

object TemplateEngineTestSet {
  type BackendName = String
  type BackendResult = String
  class Settings(val normalized: Boolean)

  object Settings {
    val default: Settings = new Settings(false)
  }
}

class ParseTest {

}

object ParseTest {
  val newlineReplacement: String = " "

  object Names {
    val rootNode: String = "test"
    val normalizeTestWhitespace: String = "normalizeTestWhitespace"
    val testInputNode: String = "input"
    val testResultElement: String = "result"
    val backendNameAttribute: String = "backend"
  }

  //don't need to handle newline characters because String.lines already
  //removes them
  private def normalizeLine(line: String): String = {
    line.trim + newlineReplacement
  }

  private def normalize(str: String): String = {
    val zero = new StringBuilder(str.length)
    str.lines.foldLeft(zero) {
      case (acc, thisLine) => {
        acc.append(normalizeLine(thisLine))
      }
    }.mkString
  }

  object Errors {
    class ParseTestError(override val msg: String)
      extends SqlppError(msg)

    class AttributeError(override val msg: String)
      extends ParseTestError(msg)

    class ElementError(override val msg: String)
      extends ParseTestError(msg)

    class UnexpectedElementError(val expected: String,
                                 val actual: Element)
      extends ElementError(s"Expected $expected but got $actual")
  }

  object ParseDocument {
    val defaultNormalizeTestWhitespaceAttribute: Boolean = false

    def parseNormalizeTestWhitespaceAttribute(root: Element):
      Either[SqlppError, Boolean] = {

      val attrName = Names.normalizeTestWhitespace

      if(root.hasAttribute(attrName)) {
        val attrValue = root.getAttribute(attrName).trim

        if(attrValue.nonEmpty) {
          Right(attrValue.toBoolean)
        } else {
          Left(new AttributeError(s"Attribute with name $attrValue is empty"))
        }
      } else {
        Right(defaultNormalizeTestWhitespaceAttribute)
      }
    }

    def parseSettings(root: Element): Either[SqlppError, TemplateEngineTestSet.Settings] = {
      parseNormalizeTestWhitespaceAttribute(root).map(new TemplateEngineTestSet.Settings(_))
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
                      (e: Element): Either[SqlppError, String] = {
        Option(e.getNodeValue).filter(_.trim.nonEmpty) match {
          case Some(x) => Right(x.trim)
          case None => Left(onError)
        }
      }
    }
    import Queries._



    def parseResultNode(resultNode: Element): Either[SqlppError, (String, String)] = {
      lazy val backendNameAttrError =
        new AttributeError(s"Expected result node to have a" +
          s" \"${Names.backendNameAttribute}\" attribute containing" +
          "the name of the backend")

      lazy val noTestResultError =
        new ElementError(s"Expected non-empty test results for $resultNode")

      for {
        _ <- expectName(Names.testResultElement, resultNode)
        backendName <- getAttribute(backendNameAttrError)(Names.backendNameAttribute, resultNode)
        testResult <- getNodeValue(noTestResultError)(resultNode)
      } yield {
        (backendName, testResult)
      }
    }

    def parseInputNode(inputNode: Element): Either[SqlppError, String] = {
      val noTestInput = new ElementError(s"Expected ${Names.testInputNode} " +
        s"$inputNode to contain the test input")
      for {
        _ <- expectName(Names.testInputNode, inputNode)
        res <- getNodeValue(noTestInput)(inputNode)
      } yield {
        res
      }
    }

    def parseInputNode(inputNodes: Seq[Element]): Either[SqlppError, String] = {
      if(inputNodes.length == 1) {
        parseInputNode(inputNodes.head)
      } else {
        Left(new ElementError(s"Expected there to be only 1 " +
          s"${Names.testInputNode} defining the test case"))
      }
    }

    def parseResultNodes(resultNodes: Seq[Element]): Either[SqlppError, Seq[(String, String)]] = {
      val empty: Either[SqlppError, Seq[(String, String)]] = Right(Seq.empty)
      resultNodes.foldLeft(empty) {
        case (eAcc, thisNode) => eAcc.flatMap { acc =>
          parseResultNode(thisNode).map(res => acc :+ res)
        }
      }
    }

    def parseRootElement(root: Element): Either[SqlppError, TemplateEngineTestSet] = {
      for {
        settings <- parseSettings(root)
        _ <- expectName(Names.rootNode, root)
        rawInputNodes = getChildrenWithName(Names.testInputNode, root)
        testInput <- parseInputNode(rawInputNodes)
        rawResultNodes = getChildrenWithName(Names.testResultElement, root)
        resultNodeValues <- parseResultNodes(rawResultNodes)
      } yield {
        new TemplateEngineTestSet(settings)(testInput, resultNodeValues.toMap)
      }
    }

    def apply(document: Document):
      Either[SqlppError, TemplateEngineTestSet] = {
      parseRootElement(document.getDocumentElement)
    }
  }


  def readTest(testLocation: InputSource): Either[SqlppError, TemplateEngineTestSet] = {
    val documentBuilderFactory = DocumentBuilderFactory.newInstance()
    val documentBuilder = documentBuilderFactory.newDocumentBuilder()
    val document = documentBuilder.parse(testLocation)

    ParseDocument(document)
  }
}
