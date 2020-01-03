package com.jakway.sqlpp.template

import java.io.{ByteArrayInputStream, File, InputStream, Writer}
import java.util.Properties

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.{IOMap, OpenOutputWriterError, TemplateEngineException}
import com.jakway.sqlpp.util._
import org.apache.velocity.Template
import org.apache.velocity.app.VelocityEngine
import org.apache.velocity.context.Context
import org.apache.velocity.exception.{ParseErrorException, ResourceNotFoundException}
import org.apache.velocity.runtime.resource.util.StringResourceRepository

import scala.util.{Failure, Success, Try}

/**
 * TODO: make a distinction between input and output encoding
 * so that we can output in different encoding from the one we
 * read in
 */
trait TemplateEngine {
  val initProperties: Properties
  val velocityEngine: VelocityEngine
  val encoding: String

  def apply(templateName: String,
            vs: ValueSource,
            output: Writer): Either[SqlppError, Unit] = {

    TemplateEngine
      .GetTemplate.getTemplateFromName(velocityEngine, templateName, encoding)
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

  def multiApplyWriters(templateName: String,
                        ioMap: IOMap): Either[SqlppError, Unit] = {
    TemplateEngine
      .GetTemplate.getTemplateFromName(velocityEngine, templateName, encoding)
      .flatMap(template => multiApplyWriters(template, ioMap))
  }

  def multiApplyWriters(template: Template,
            ioMap: IOMap): Either[SqlppError, Unit] = {
    val empty: Either[SqlppError, Unit] = Right({})
    ioMap.foldLeft(empty) {
      case (eAcc, (vs, output)) => eAcc.flatMap { acc =>
        for {
          _ <- apply(template, vs, output)
          _ <- TemplateEngine.genericWrapTry(Try(output.close()))
        } yield {}
      }
    }
  }

  def multiApplyFiles(templateName: String,
                      ioMap: Map[ValueSource, File]): Either[SqlppError, Unit] = {
    val empty: Either[SqlppError, Map[ValueSource, Writer]] = Right(Map.empty)
    val writerMap = ioMap.foldLeft(empty) {
      case (eAcc, (vs, dest)) => eAcc.flatMap { acc =>
        openOutput(dest)
          .map(output => acc.updated(vs, output))
      }
    }

    writerMap.flatMap(multiApplyWriters(templateName, _))
  }

  def loadTemplateFromInputStream: InputStream =>
        String =>
        String =>
        String =>
        Either[SqlppError, Template] =
    (TemplateEngine.GetTemplate.getTemplateFromInputStream _)
      .curried(velocityEngine)

  def loadTemplateFromString: String =>
        String =>
        String =>
        String =>
        Either[SqlppError, Template] = {
    templateStr => templateSourceKey => repositoryName => encoding =>

      val asInputStream = new ByteArrayInputStream(
        templateStr.getBytes(encoding))

      loadTemplateFromInputStream(
        asInputStream)(
        templateSourceKey)(
        repositoryName)(
        encoding)
  }


  private def openOutput: File => Either[SqlppError, Writer] =
    FileUtil.openWriter(encoding, new OpenOutputWriterError(_))
}

object TemplateEngine {
  type IOMap = Map[ValueSource, Writer]
  type PropertyMap = Map[String, String]

  case class ExtraTemplateOptions(stringRepositoryName: String,
                                  extraDirs: Seq[File],
                                  extraJars: Seq[String]) {
    def dirsToStrings: Seq[String] = extraDirs.map(_.getAbsolutePath)
  }

  class TemplateEngineError(override val msg: String)
    extends SqlppError(msg)

  class TemplateOutputError(override val msg: String)
    extends SqlppError(msg)

  class OpenOutputWriterError(override val msg: String)
    extends TemplateOutputError(msg) {
    def this(t: Throwable) {
      this(SqlppError.formatThrowable(t))
    }
  }

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

  class StringResourceRepositoryError(override val msg: String)
    extends TemplateEngineError(msg)

  object GetTemplate {
    def getTemplateFromName(engine: VelocityEngine,
              templateName: String,
              encoding: String): Either[SqlppError, Template] = {
      for {
        _ <- checkTemplateExists(engine, templateName)
        res <- getTemplate(engine, templateName, encoding)
      } yield {
        res
      }
    }

    def getTemplateFromInputStream(engine: VelocityEngine,
              templateSource: InputStream,
              templateSourceKey: String,
              repositoryName: String,
              encoding: String): Either[SqlppError, Template] = {
      for {
        templateString <- readStream(templateSource, encoding)
        repo <- getStringResourceRepository(engine, repositoryName)
        _ <- addTemplateString(engine,
          templateString,
          templateSourceKey,
          repo,
          encoding)
        template <- getTemplateFromName(engine, templateSourceKey, encoding)
      } yield {
        template
      }
    }

    /**
     * add the string as a resource
     */
    private def addTemplateString(engine: VelocityEngine,
                                  templateString: String,
                                  key: String,
                                  repo: StringResourceRepository,
                                  encoding: String):
      Either[SqlppError, Unit] = {

      def onError: Throwable => SqlppError = { t =>
        new StringResourceRepositoryError(
          s"Error adding template string < $templateString > " +
            s" to StringResourceRepository < $repo >, " +
            s"caused by: " + SqlppError.formatThrowableCause(t))
      }

      TryToEither.apply(onError) {
        Try {
          repo.putStringResource(key, templateString, encoding)
        }
      }
    }

    private def readStream(inputStream: InputStream, encoding: String):
      Either[SqlppError, String] = {

      def onError: Throwable => SqlppError = { t =>
        new StringResourceRepositoryError(
          s"Error reading template source into a String," +
            s" caused by: " + SqlppError.formatThrowableCause(t))
      }

      TryToEither.apply(onError) {
        Try(scala.io.Source.fromInputStream(inputStream, encoding).mkString)
      }
    }

    private def getStringResourceRepository(engine: VelocityEngine,
                                            repositoryName: String):
      Either[SqlppError, StringResourceRepository] = {

      Option(engine.getApplicationAttribute(repositoryName)) match {
        case Some(stringResourceRepository) => {
          stringResourceRepository match {
            case repository: StringResourceRepository =>
              Right(repository)

            case _ => Left(new StringResourceRepositoryError(
              s"Found a non-null repository under the name $repositoryName" +
              " but it's not an instance of StringResourceRepository"
            ))
          }
        }

        case None => Left(new StringResourceRepositoryError(
          s"Could not find a StringResourceRepository instance under the" +
            s" name $repositoryName"
        ))
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

  private def genericWrapTry[A](f: => Try[A]): Either[SqlppError, A] = {
    f match {
      case Success(x) => Right(x)
      case Failure(t) => Left(new TemplateEngineException(t))
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
              resourceLoaderTypes)(
              extraTemplateOptions.stringRepositoryName)(
              extraTemplateOptions.dirsToStrings)(
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

  def apply: String =>
             Set[LoaderType] =>
             ExtraTemplateOptions =>
             PropertyMap =>
             Either[SqlppError, TemplateEngine] = Constructor.apply
}
