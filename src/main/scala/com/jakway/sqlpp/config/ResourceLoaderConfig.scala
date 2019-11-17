package com.jakway.sqlpp.config

import java.util.Properties

import com.jakway.sqlpp.config.Config.ConfigError
import com.jakway.sqlpp.config.ResourceLoaderConfig.ResourceLoaderProperties.ToPropertiesError
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.util.{MapToProperties, MergeMaps}
import org.apache.velocity.runtime.{RuntimeConstants => VelocityConstants}

import scala.util.{Failure, Success}

/**
 * sets up velocity resource loader properties
 * see http://velocity.apache.org/engine/1.7/developer-guide.html#configuring-resource-loaders
 */
object ResourceLoaderConfig {
  type PropertyMap = Map[String, String]

  class ResourceLoaderConfigError(override val msg: String)
    extends ConfigError(msg)

  private val defaultMultipleEntriesSeparator: String = ", "

  private def multipleEntries(entries: Seq[String],
                              separator: String =
                                defaultMultipleEntriesSeparator): String = {
    entries.reduce(_ + separator + _)
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
          VelocityConstants.RESOURCE_LOADER,
          getLoaderNamesEntry)


      //will be edited in-place by MapToProperties
      val ret = new Properties()
      MapToProperties(finalPropertyMap)(ret) match {

        case Success(overwrittenValues) => {
          //no entries should have been overwritten
          if(overwrittenValues.isEmpty) {
            Right(ret)
          } else {
            Left(new ToPropertiesError(s"Unexpected overwritten values" +
              s" $overwrittenValues in properties map $ret"))
          }
        }

        case Failure(t) => new ToPropertiesError(
          s"Error caused by exception: " + SqlppError.formatThrowable(t))
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

}
