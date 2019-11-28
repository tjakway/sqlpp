package com.jakway.sqlpp.template

import java.io.{File, Writer}
import java.util.Properties

import com.jakway.sqlpp.ValueSource
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.TemplateEngineException
import com.jakway.sqlpp.util.{MapToProperties, MergeMaps, MergeProperties}
import org.apache.velocity.Template
import org.apache.velocity.app.VelocityEngine
import org.apache.velocity.context.Context
import org.apache.velocity.exception.{ParseErrorException, ResourceNotFoundException}

import scala.util.{Failure, Success, Try}

trait TemplateEngine {
  val initProperties: Properties
  val velocityEngine: VelocityEngine
  val encoding: String

  def apply(templateName: String,
            vs: ValueSource,
            output: Writer): Either[SqlppError, Unit] = {

    TemplateEngine
      .GetTemplate(velocityEngine, templateName, encoding)
      .flatMap(apply(_, vs, output))
  }

  def apply(velocityTemplate: Template,
            vs: ValueSource,
            output: Writer): Either[SqlppError, Unit] = Try {

    for {
      vc <- vs.toVelocityContext
      _ <- TemplateEngine.merge(velocityTemplate, vc, output)
    } yield {{}}
  } match {
    case Success(x) => x
    case Failure(t) => Left(new TemplateEngineException(t))
  }
}

object TemplateEngine {
  type PropertyMap = Map[String, String]

  case class ExtraTemplateOptions(extraDirs: Seq[File],
                                  extraJars: Seq[String]) {
    def dirsToStrings: Seq[String] = extraDirs.map(_.getAbsolutePath)
  }

  class TemplateEngineError(override val msg: String)
    extends SqlppError(msg)

  class TemplateEngineException(val throwable: Throwable)
    extends TemplateEngineError(SqlppError.formatThrowable(throwable))

  class TemplateNotFoundError(override val msg: String)
    extends TemplateEngineError(msg) {
    def this(t: Throwable) {
      this(SqlppError.formatThrowable(t))
    }
  }

  class TemplateParseError(override val msg: String)
    extends TemplateEngineError(msg) {
    def this(t: Throwable) {
      this(SqlppError.formatThrowable(t))
    }
  }

  object GetTemplate {
    def apply(engine: VelocityEngine,
              templateName: String,
              encoding: String): Either[SqlppError, Template] = {
      for {
        _ <- checkTemplateExists(engine, templateName)
        res <- getTemplate(engine, templateName, encoding)
      } yield {
        res
      }
    }


    private def checkTemplateExists(engine: VelocityEngine,
                            templateName: String): Either[SqlppError, Unit] = {
      if(engine.resourceExists(templateName)) {
        Right({})
      } else {
        Left(new TemplateNotFoundError(s"Could not find template $templateName"))
      }
    }

    private def getTemplate(engine: VelocityEngine,
                    templateName: String,
                    encoding: String): Either[SqlppError, Template] = {
      Try(engine.getTemplate(templateName, encoding)) match {
        case Success(x) => Right(x)
        case Failure(t) => Left(new TemplateEngineException(t))
      }
    }
  }


  /**
   * wraps issue-specific exceptions in SqlppError subclasses on failure
   * @param velocityTemplate
   * @param context
   * @param writer
   * @return
   */
  private def merge(velocityTemplate: Template,
            context: Context,
            writer: Writer): Either[SqlppError, Unit] = Try {
    velocityTemplate.merge(context, writer)
  } match {
    case Success(_) => Right({})
    case Failure(t) => Left(t match {
      case _: ResourceNotFoundException =>
        new TemplateNotFoundError(t)
      case _: ParseErrorException =>
        new TemplateParseError(t)
      case _ =>
        new TemplateEngineException(t)
    })
  }


  private object Constructor {
    class TemplateEngineConstructorError(override val msg: String)
      extends TemplateEngineError(msg)

    class LoadConfigVelocityPropertiesError(val propertyMap: PropertyMap,
                                            val throwable: Throwable)
      extends TemplateEngineConstructorError(
        s"There was an error loading config.additionalVelocityProperties $propertyMap: " +
        SqlppError.formatThrowable(throwable))


    class MergePropertiesError(override val msg: String)
      extends TemplateEngineConstructorError(msg)

    object MergePropertiesError {
      def handleException: Throwable => SqlppError = { (t: Throwable) =>
        new MergePropertiesError(
          s"Error while merging properties caused by exception: " +
          SqlppError.formatThrowable(t))
      }
    }

    class DuplicatePropertiesError(override val msg: String)
      extends MergePropertiesError(msg)

    object DuplicatePropertiesError {
      def handleDuplicates: MergeMaps.HandleDuplicatesF[
        MergeProperties.KeyType,
        MergeProperties.ValueType,
        SqlppError] = (key: String) => (leftValue: String) => (rightValue: String) => {

        Left(new DuplicatePropertiesError(
          s"Unexpected duplicate properties " +
            s"with key $key: ($leftValue, $rightValue)"))
      }
    }

    private def additionalVelocityPropertiesToPropertiesObject(additionalVelocityProperties: PropertyMap):
      Either[SqlppError, Properties] = {

      MapToProperties.withNewProperties(additionalVelocityProperties) match {
        case Success(x) => Right(x)
        case Failure(t) => Left(new LoadConfigVelocityPropertiesError(
          additionalVelocityProperties, t))
      }
    }

    /**
     * get the loader properties and merge them with the additional properties
     * listed in config
     * @return
     */
    private def getMergedProperties(resourceLoaderTypes: Set[LoaderType])
                                   (extraTemplateOptions: ExtraTemplateOptions)
                                   (additionalVelocityProperties: PropertyMap):
    Either[SqlppError, Properties] = {
      for {
        loaderProperties <- ResourceLoaderConfig
            .StandardResourceLoaders
            .getCombinedProperties(
              resourceLoaderTypes,
              extraTemplateOptions.dirsToStrings,
              extraTemplateOptions.extraJars)
        additionalOptions <- additionalVelocityPropertiesToPropertiesObject(additionalVelocityProperties)

        mergedProperties <- MergeProperties.merge(
          loaderProperties,
          additionalOptions,
          DuplicatePropertiesError.handleDuplicates,
          MergePropertiesError.handleException)
      } yield {
        mergedProperties
      }
    }

    private def getVelocityEngine: Properties => Either[SqlppError, VelocityEngine] = {
      (prop: Properties) => {
        val ve = new VelocityEngine(prop)
        ve.init(prop)
        Right(ve)
      }
    }


    def apply: String =>
               Set[LoaderType] =>
               ExtraTemplateOptions =>
               PropertyMap =>
               Either[SqlppError, TemplateEngine] = {
      encodingP =>
        loaderTypes =>
        extraTemplateOptions =>
        additionalVelocityProperties =>
      for {
        mergedProperties <- getMergedProperties(
          loaderTypes)(extraTemplateOptions)(additionalVelocityProperties)

        engine <- getVelocityEngine(mergedProperties)
      } yield {
        new TemplateEngine {
          override val initProperties: Properties = mergedProperties
          override val velocityEngine: VelocityEngine = engine
          override val encoding: String = encodingP
        }
      }
    }
  }

  def apply = Constructor.apply
}
