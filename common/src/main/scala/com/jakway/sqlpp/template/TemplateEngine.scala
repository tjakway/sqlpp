package com.jakway.sqlpp.template

import java.io.File
import java.util.Properties

import com.jakway.sqlpp.ValueSource
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
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

  case class ExtraTemplateOptions(extraDirs: Seq[File],
                                  extraJars: Seq[String]) {
    def dirsToStrings: Seq[String] = extraDirs.map(_.getAbsolutePath)
  }

  class TemplateEngineError(override val msg: String)
    extends SqlppError(msg)

  class TemplateNotFoundError(override val msg: String)
    extends TemplateEngineError(msg)

  def checkTemplateExists(engine: VelocityEngine,
                          templateName: String): Either[SqlppError, Unit] = {
      if(engine.resourceExists(templateName)) {
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


    def apply: Set[LoaderType] =>
               ExtraTemplateOptions =>
               PropertyMap =>
               Either[SqlppError, TemplateEngine] = {
      loaderTypes => extraTemplateOptions => additionalVelocityProperties =>
      for {
        mergedProperties <- getMergedProperties(
          loaderTypes)(extraTemplateOptions)(additionalVelocityProperties)

        engine <- getVelocityEngine(mergedProperties)
      } yield {
        new TemplateEngine {
          override val initProperties: Properties = mergedProperties
          override val velocityEngine: VelocityEngine = engine
        }
      }
    }
  }

  def apply = Constructor.apply
}
