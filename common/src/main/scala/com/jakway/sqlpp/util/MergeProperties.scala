package com.jakway.sqlpp.util

import java.util.Properties

import scala.util.{Failure, Success, Try}

object MergeProperties {
  type KeyType = String
  type ValueType = String
  type HandleDuplicatesF[ErrorType] = MergeMaps.HandleDuplicatesF[KeyType, ValueType, ErrorType]

  def merge[ErrorType](
            left: Properties,
            right: Properties,
            handleDuplicatesF: HandleDuplicatesF[ErrorType],
            handleException: Throwable => ErrorType):
  Either[ErrorType, Properties] = {
    def tryToEither[A]: Try[A] => Either[ErrorType, A] = {
      case Success(x) => Right(x)
      case Failure(t) => Left(handleException(t))
    }

    //Properties -> Map
    def convProperties: Properties => Either[ErrorType, Map[KeyType, ValueType]] =
      (prop: Properties) => tryToEither(propertiesToMap(prop))

    //Map -> Properties
    def convMap: Map[KeyType, ValueType] => Either[ErrorType, Properties] = {
      (map: Map[KeyType, ValueType]) =>
        tryToEither(MapToProperties.withNewProperties(map))
    }

    for {
      leftMap <- convProperties(left)
      rightMap <- convProperties(right)
      mergedPropertiesAsMap <- MergeMaps.mergeMaps(handleDuplicatesF)(leftMap, rightMap)
      mergedProperties <- convMap(mergedPropertiesAsMap)
    } yield {
      mergedProperties
    }
  }

  def propertiesToMap(prop: Properties): Try[Map[KeyType, ValueType]] = Try {
    import scala.collection.JavaConverters._
    prop
      .keySet()
      .asScala
      .foldLeft(Map.empty: Map[KeyType, ValueType]) {
      case (acc, thisPropName) => {
        //ensure it returns a valid object by calling toString on the returned String
        //Properties returns null if they key isn't found
        acc.updated(thisPropName.toString, prop.getProperty(thisPropName.toString).toString)
      }
    }
  }
}
