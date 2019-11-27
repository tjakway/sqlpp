package com.jakway.sqlpp

import java.io.File
import java.util.Properties

import com.jakway.sqlpp.config.ResourceLoaderConfig.PropertyMap
import com.jakway.sqlpp.config.{Config, OutputTarget, ResourceLoaderConfig}
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.util.{MapToProperties, MergeMaps, MergeProperties}
import org.apache.velocity.app.VelocityEngine

import scala.util.{Failure, Success}

trait TemplateEngine {
  val initProperties: Properties
  val velocityEngine: VelocityEngine

  def apply(templateName: String,
            vs: ValueSource,
            output: File): Either[SqlppError, File] = {

    for {
      _ <- TemplateEngine.checkTemplateExists(velocityEngine, templateName)
      vc <- vs.toVelocityContext
      
    } yield {

    }
    ???
  }
}

object TemplateEngine {
  type PropertyMap = Map[String, String]

  class TemplateEngineError(override val msg: String)
    extends SqlppError(msg)

  class TemplateNotFoundError(override val msg: String)
    extends TemplateEngineError(msg)

  def checkTemplateExists(engine: VelocityEngine,
                          templateName: String): Either[SqlppError, Unit] = {
      if(engine.templateExists(templateName)) {
        Right({})
      } else {
        Left(new TemplateNotFoundError(s"Could not find template $templateName"))
      }
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

    private def additionalVelocityPropertiesToPropertiesObject(config: Config):
      Either[SqlppError, Properties] = {

      MapToProperties.withNewProperties(config.additionalVelocityProperties) match {
        case Success(x) => Right(x)
        case Failure(t) => Left(new LoadConfigVelocityPropertiesError(
          config.additionalVelocityProperties, t))
      }
    }

    /**
     * get the loader properties and merge them with the additional properties
     * listed in config
     * @param config
     * @return
     */
    private def getMergedProperties(config: Config): Either[SqlppError, Properties] = {
      for {
        loaderProperties <- ResourceLoaderConfig
            .StandardResourceLoaders
            .getCombinedProperties(
              config.resourceLoaderTypes,
              config.extraTemplateOptions.dirsToStrings,
              config.extraTemplateOptions.extraJars)
        additionalOptions <- additionalVelocityPropertiesToPropertiesObject(config)

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

    def apply: Config => Either[SqlppError, TemplateEngine] = { config =>
      for {
        mergedProperties <- getMergedProperties(config)
        engine <- getVelocityEngine(mergedProperties)
      } yield {
        new TemplateEngine {
          override val initProperties: Properties = mergedProperties
          override val velocityEngine: VelocityEngine = engine
        }
      }
    }
  }


  def apply: Config => Either[SqlppError, TemplateEngine] =
    Constructor.apply
}
