package com.jakway.sqlpp.template

import java.util.Properties

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.ResourceLoaderConfig.ResourceLoaderProperties.ToPropertiesError
import com.jakway.sqlpp.template.TemplateEngine.PropertyMap
import com.jakway.sqlpp.util.{MapToProperties, MergeMaps}

import scala.util.{Failure, Success}
import org.apache.velocity.runtime.{RuntimeConstants => VelocityConstants}

/**
 * sets up velocity resource loader properties
 * see http://velocity.apache.org/engine/1.7/developer-guide.html#configuring-resource-loaders
 *
 * You probably want to use StandardResourceLoaders.getCombinedProperties and pass the result
 * to VelocityEngine
 */
object ResourceLoaderConfig {
  class ResourceLoaderConfigError(override val msg: String)
    extends SqlppError(msg)

  private val defaultMultipleEntriesSeparator: String = ", "

  private def multipleEntries(entries: Seq[String],
                              separator: String =
                                defaultMultipleEntriesSeparator): String = {
    entries.reduceOption(_ + separator + _).getOrElse("")
  }

  class ResourceLoaderProperties(val loaderNames: Set[String],
                                 val props: PropertyMap) {
    def merge(other: ResourceLoaderProperties):
      Either[SqlppError, ResourceLoaderProperties] = {

      //merge each's property map, which shouldn't conflict
      ResourceLoaderProperties.mergePropertyMaps(props, other.props)
        .map { newProps =>
          new ResourceLoaderProperties(
            loaderNames ++ other.loaderNames,
            newProps)
        }
    }

    private def getLoaderNamesEntry: String = multipleEntries(loaderNames.toSeq)

    def toPropertiesObject: Either[SqlppError, Properties] = {
      //the property map with the combined loader entry
      val finalPropertyMap =
        props.updated(
          VelocityConstants.RESOURCE_LOADERS,
          getLoaderNamesEntry)


      //will be edited in-place by MapToProperties
      val ret = new Properties()
      MapToProperties(finalPropertyMap, ret) match {

        case Success(overwrittenValues) => {
          //no entries should have been overwritten
          if(overwrittenValues.isEmpty) {
            Right(ret)
          } else {
            Left(new ToPropertiesError(s"Unexpected overwritten values" +
              s" $overwrittenValues in properties map $ret"))
          }
        }

        case Failure(t) => Left(new ToPropertiesError(
          s"Error caused by exception: " + SqlppError.formatThrowable(t)))
      }
    }
  }

  object ResourceLoaderProperties {
    class UnexpectedDuplicateEntryError(val entryName: String,
                                        val entries: (String, String))
      extends ResourceLoaderConfigError(s"Unexpected duplicate entries for " +
        s"$entryName: " + entries)

    class ToPropertiesError(override val msg: String)
      extends ResourceLoaderConfigError(msg)


    def mergePropertyMaps(left: PropertyMap, right: PropertyMap):
      Either[SqlppError, PropertyMap] = {

      MergeMaps.mergeMaps(duplicatesUnexpected)(left, right)
    }

    def duplicatesUnexpected: MergeMaps.HandleDuplicatesF[
      String, String, SqlppError] = { key => leftV => rightV =>

      Left(new UnexpectedDuplicateEntryError(key, (leftV, rightV)))
    }
  }

  object StandardResourceLoaders {
    private val modificationCheckIntervalString: String = "modification_check_interval"

    class StandardResourceLoaderError(override val msg: String)
      extends ResourceLoaderConfigError(msg)

    class NoResourceLoaderTypesPassed
      extends StandardResourceLoaderError("Must pass at least one object of LoaderType")


    //entries from http://velocity.apache.org/engine/1.7/developer-guide.html#configuring-resource-loaders
    private def standardEntries(name: String)
                       (description: String,
                        fullyQualifiedClassName: String,
                        pathOption: Option[Seq[String]]): PropertyMap = {
      val m = Map(
        s"resource.loader.$name.description" -> description,
        s"resource.loader.$name.class" -> fullyQualifiedClassName
      )

      pathOption match {
        case Some(path) => m.updated(s"resource.loader.$name.path", multipleEntries(path))
        case None => m
      }
    }

    private def fileEntries(name: String): PropertyMap = {
      Map(
        s"resource.loader.$name.cache" -> "false",
        s"resource.loader.$name.$modificationCheckIntervalString" -> "0"
      )
    }

    private lazy val stringResourceRepositoryImplClass: String =
      classOf[org.apache.velocity.runtime.resource.util.StringResourceRepositoryImpl]
        .getName


    private lazy val stringResourceLoaderClass: String =
      classOf[org.apache.velocity.runtime.resource.loader.StringResourceLoader]
        .getName

    private def stringLoaderEntries(stringRepositoryName: String):
      PropertyMap = Map(

      //see https://velocity.apache.org/engine/2.0/apidocs/org/apache/velocity/runtime/resource/loader/StringResourceLoader.html
      //for examples of these entries
      //(note that repository.name is generated by ResourceLoaderProperties)
      "resource.loader.string.repository.name" -> stringRepositoryName,
      "resource.loader.string.description" ->
        s"SQLPP String Resource Loader << $stringRepositoryName >>",

      "resource.loader.string.class" -> stringResourceLoaderClass,
      "resource.loader.string.repository.class" ->
        stringResourceRepositoryImplClass,

      //need to call velocityEngine.getApplicationAttribute(stringRepositoryName)
      //to get the repository
      "resource.loader.string.repository.static" -> "false",
    )

    sealed trait LoaderType {
      val loaderName: String
    }

    object LoaderType {
      val all: Set[LoaderType] = Set(FileLoader, ClassLoader, JarLoader)

      class LoaderTypeReadError(override val msg: String)
        extends StandardResourceLoaderError(msg)

      private def printLoaderNames: String = {
        val closingBrace: String = {
          //prevent double spaces for the empty set
          val prefix = if(all.size > 1) {
            " "
          } else {
            ""
          }
          prefix + "}"
        }

        "{ " + all.map(_.loaderName).reduce(_ + ", " + _) + closingBrace
      }

      private def cmpTrimmedStrings: (String, String) => Boolean = {
        case (left, right) => {
          left.trim == right.trim
        }
      }

      def read(loaderTypeName: String,
               cmp: (String, String) => Boolean = cmpTrimmedStrings): Either[SqlppError, LoaderType] = {
        all.find(x => cmp(x.loaderName, loaderTypeName)) match {
          case Some(x) => Right(x)
          case None =>
            Left(new LoaderTypeReadError(
              s"Could not find a resource loader matching" +
                s" passed name < " + loaderTypeName + " >" +
                " in " + printLoaderNames))
        }
      }
    }

    case object FileLoader extends LoaderType {
      override val loaderName: String = "file"
    }

    case object ClassLoader extends LoaderType {
      override val loaderName: String = "class"
    }

    case object JarLoader extends LoaderType {
      override val loaderName: String = "jar"
    }

    case object StringLoader extends LoaderType {
      override val loaderName: String = "string"
    }

    /**
     * get a resource loader configured to look in the passed directories
     *
     * @param forDirs
     * @return
     */
    private def getFileResourceLoader(forDirs: Seq[String]): Either[SqlppError, ResourceLoaderProperties] = {
      ResourceLoaderProperties.mergePropertyMaps(
        standardEntries(FileLoader.loaderName)(
          "Velocity File Resource Loader",
          "org.apache.velocity.runtime.resource.loader.FileResourceLoader",
          Some(forDirs)),
        fileEntries(FileLoader.loaderName)
      ).map { propMap =>
        new ResourceLoaderProperties(Set(FileLoader.loaderName), propMap)
      }
    }

    private def getClassResourceLoader: Either[SqlppError, ResourceLoaderProperties] =
      Right(new ResourceLoaderProperties(Set(ClassLoader.loaderName),
        standardEntries(ClassLoader.loaderName)(
          "Velocity Classpath Resource Loader",
          "org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader",
          None)))

    private def getJarResourceLoader(forJars: Seq[String]): Either[SqlppError, ResourceLoaderProperties] =
      Right(new ResourceLoaderProperties(Set(JarLoader.loaderName),
        standardEntries(JarLoader.loaderName)(
          "Velocity Jar Resource Loader",
          "org.apache.velocity.runtime.resource.loader.JarResourceLoader",
          Some(forJars))))

    private def getStringResourceLoader(stringRepositoryName: String):
      Either[SqlppError, ResourceLoaderProperties] = {

      Right(new ResourceLoaderProperties(Set(StringLoader.loaderName),
        stringLoaderEntries(stringRepositoryName)
      ))
    }



    /**
     * remaining arguments are ignored if not relevant to the passed loader type
     */
    private def loaderTypeToResourceLoaderProperties: GetLoaderF[
      LoaderType,
      ResourceLoaderProperties] = {

      forLoaderType => stringRepositoryName => dirs => jars =>

      forLoaderType match {
        case FileLoader => getFileResourceLoader(dirs)
        case ClassLoader => getClassResourceLoader
        case JarLoader => getJarResourceLoader(jars)
        case StringLoader => getStringResourceLoader(stringRepositoryName)
      }
    }


    type GetLoaderF[LType, ResType] = LType =>
                             String =>
                             Seq[String] =>
                             Seq[String] =>
                             Either[SqlppError, ResType]

    def getCombinedLoader: GetLoaderF[Set[LoaderType], ResourceLoaderProperties] = {
      forLoaderTypes => stringRepositoryName => dirs => jars =>


      val toPropertiesE = {
        val empty: Either[SqlppError, Set[ResourceLoaderProperties]] = Right(Set())
        val res = forLoaderTypes.foldLeft(empty) {
          case (eAcc, thisLoaderType) => eAcc.flatMap { acc =>
            loaderTypeToResourceLoaderProperties(thisLoaderType)(
              stringRepositoryName)(dirs)(jars)
              .map(res => acc + res)
          }
        }

        res.map(_.toSeq)
      }

      toPropertiesE.flatMap { toProperties =>
        //divide into head and tail so we can start with the head and fold without needing a zero element
        toProperties.headOption match {
          case Some(head) => {
            val start: Either[SqlppError, ResourceLoaderProperties] = Right(head)
            toProperties.tail.foldLeft(start) {
              case (eAcc, properties) => eAcc.flatMap { acc =>
                acc.merge(properties)
              }
            }
          }
          //need to request at least one resource loader
          case None => Left(new NoResourceLoaderTypesPassed())
        }
      }
    }

    /**
     * ***************************
     * *****Class entry point*****
     * ***************************
     */
    def getCombinedProperties: GetLoaderF[Set[LoaderType], Properties] = {
      forLoaderTypes => stringRepositoryName => dirs => jars =>

      getCombinedLoader(forLoaderTypes)(stringRepositoryName)(dirs)(jars)
        .flatMap(_.toPropertiesObject)
    }
  }
}
