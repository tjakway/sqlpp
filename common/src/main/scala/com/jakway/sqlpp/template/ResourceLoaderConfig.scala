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

    class StandardResourceLoaderError(override val msg: String)
      extends ResourceLoaderConfigError(msg)

    class NoResourceLoaderTypesPassed
      extends StandardResourceLoaderError("Must pass at least one object of LoaderType")




    sealed trait LoaderType {
      def toPropertyMap(forDirs: Seq[String],
                        forJars: Seq[String]): Either[SqlppError, PropertyMap]
    }

    object LoaderType {
      val all: Set[LoaderType] = Set(FileLoader, ClassLoader, JarLoader)

      //entries from http://velocity.apache.org/engine/1.7/developer-guide.html#configuring-resource-loaders
      def withStandardEntries(name: String,
                              description: String,
                              fullyQualifiedClassName: String,
                              pathOption: Option[Seq[String]]):
      PropertyMap = {

        val m = Map {
          s"resource.loader.$name.description" -> description
          s"resource.loader.$name.class" -> fullyQualifiedClassName
        }

        pathOption match {
          case Some(path) => m.updated(s"$name.resource.loader.path", multipleEntries(path))
          case None => m
        }
      }

      def fileEntries(name: String): PropertyMap = {
        Map {
          s"resource.loader.$name.cache" -> "false"
          s"resource.loader.$name.modificationCheckInterval" -> "0"
        }
      }
    }

    case object FileLoader extends LoaderType {

      /**
       * @param forDirs
       * @param forJars ignored
       * @return
       */
      override def toPropertyMap(forDirs: Seq[String],
                                 forJars: Seq[String]):
      Either[SqlppError, PropertyMap] = {
        val loaderName: String = "file"
        val description: String = "Velocity File Resource Loader"
        val className: String =
          "org.apache.velocity.runtime.resource.loader.FileResourceLoader"

        ResourceLoaderProperties.mergePropertyMaps(
          LoaderType.withStandardEntries(
            loaderName,
            description,
            className,
            Some(forDirs)),
          LoaderType.fileEntries(loaderName)
        )
      }
    }

    case object ClassLoader extends LoaderType {
      override def toPropertyMap(
                                  forDirs: Seq[String],
                                  forJars: Seq[String]):
      Either[SqlppError, PropertyMap] = {
        val loaderName: String = "class"
        val description: String = "Velocity Classpath Resource Loader"
        val className: String =
          "org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader"

        LoaderType.withStandardEntries(
          loaderName,
          description,
          className,
          None)

        /*

      Right(new ResourceLoaderProperties(Set(ClassLoader.loaderName),
        standardEntries(ClassLoader.loaderName)(
          "Velocity Classpath Resource Loader",
          "org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader",
          None)))
         */
      }
    }

    case object JarLoader extends LoaderType {
      override val loaderName: String = "jar"
    }

    case object StringLoader extends LoaderType {
      override val loaderName: String = "stringloader"
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

    /**
     * remaining arguments are ignored if not relevant to the passed loader type
     *
     * @param forLoaderType
     * @param dirs
     * @param jars
     * @return
     */
    private def loaderTypeToResourceLoaderProperties(
                                                      forLoaderType: LoaderType,
                                                      dirs: Seq[String],
                                                      jars: Seq[String]): Either[SqlppError, ResourceLoaderProperties] = {

      forLoaderType match {
        case FileLoader => getFileResourceLoader(dirs)
        case ClassLoader => getClassResourceLoader
        case JarLoader => getJarResourceLoader(jars)
      }
    }


    def getCombinedLoader(forLoaderTypes: Set[LoaderType],
                          dirs: Seq[String],
                          jars: Seq[String]): Either[SqlppError, ResourceLoaderProperties] = {


      val toPropertiesE = {
        val empty: Either[SqlppError, Set[ResourceLoaderProperties]] = Right(Set())
        val res = forLoaderTypes.foldLeft(empty) {
          case (eAcc, thisLoaderType) => eAcc.flatMap { acc =>
            loaderTypeToResourceLoaderProperties(thisLoaderType, dirs, jars)
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
     *
     * *****Class entry point*****
     *
     * @param forLoaderTypes
     * @param dirs
     * @param jars
     * @return
     */
    def getCombinedProperties(forLoaderTypes: Set[LoaderType],
                          dirs: Seq[String],
                          jars: Seq[String]): Either[SqlppError, Properties] = {
      getCombinedLoader(forLoaderTypes, dirs, jars)
        .flatMap(_.toPropertiesObject)
    }
  }
}
