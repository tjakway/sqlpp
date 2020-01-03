package com.jakway.sqlpp.config.test.template

import com.jakway.sqlpp.config.test.WithDefaultTestConfig
import com.jakway.sqlpp.config.test.util.TemplateTestUtil
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine
import org.apache.velocity.Template
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

/**
 * TODO: generate repositoryName (will require generating testConfig)
 */
class StringRepositoryProperties
  extends AnyPropSpec
    with Matchers
    with WithDefaultTestConfig
    with TemplateTestUtil {
  import StringRepositoryProperties._
  import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

  private val loaderTypes: Set[LoaderType] =
    Set(StandardResourceLoaders.StringLoader)

  property("Insert and retrieve the generated strings") {
    forAll(genStringRepoTest(
      Gen.const(
        testConfig.templateStringInfo.stringResourceRepositoryName),
      Gen.const(testConfig.encoding))) { test =>

      val res = for {
        templateEngine <- getTemplateEngine(testConfig)
        _ <- insertAllTemplates(
          templateEngine,
          test.toProcess,
          test.repositoryName,
          test.encoding)
      } yield {}

      res should be ('right)
    }
  }

}

object StringRepositoryProperties {
  case class StringRepoTest(toProcess: Map[String, String],
                            repositoryName: String,
                            encoding: String)

  def genStringRepoTest(genRepositoryName: Gen[String],
                        genEncoding: Gen[String]): Gen[StringRepoTest] = {
    genRepositoryName.flatMap { repositoryName =>
      genEncoding.flatMap { encoding =>
        Gen.listOf(Gen.alphaNumStr).map { values =>
          val m: Map[String, String] = values.map { thisValue =>
            //unfortunately no elegant way to return errors in a Gen,
            //so just throw if getTemplateSourceHash fails
            (TemplateTestUtil.getTemplateSourceHash(thisValue).right.get, thisValue)
          }.toMap

          StringRepoTest(m, repositoryName, encoding)
        }
      }
    }
  }

  private def insertAllTemplates(templateEngine: TemplateEngine,
                         templateSources: Map[String, String],
                         repositoryName: String,
                         encoding: String):
    Either[SqlppError, Map[String, Template]] = {

    val zero: Either[SqlppError, Map[String, Template]] = Right(Map.empty)
    templateSources.foldLeft(zero) {
      case (eAcc, (key, thisTemplateSource)) => eAcc.flatMap { acc =>
        templateEngine.loadTemplateFromString(thisTemplateSource)(
          key)(repositoryName)(encoding)
          .map(template => acc.updated(key, template))
      }
    }
  }
}