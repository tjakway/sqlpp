package com.jakway.sqlpp.util

import java.util.Properties

import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

object MergeProperties {
  type KeyType = String
  type ValueType = String
  type HandleDuplicatesF[ErrorType] = MergeMaps.HandleDuplicatesF[KeyType, ValueType, ErrorType]

  class HandleDuplicatesFunctions[ErrorType](val mkError: String => ErrorType,
                                             val enableLogging: Boolean
                                               = HandleDuplicatesFunctions
                                                  .defaultEnableLogging) {
    private val logger: Logger = LoggerFactory.getLogger(getClass)

    private def log(key: KeyType,
                    left: ValueType,
                    right: ValueType,
                    chose: Option[ValueType]): Unit = {
      if(enableLogging) {
        val choseMsg: String =
          chose.map(c => s"; chose $c")
               .getOrElse("")

        logger.debug(
          s"Found duplicate values for key $key: ($left, $right)"
            + choseMsg)
      } else {}
    }

    def noDuplicates: HandleDuplicatesF[ErrorType] = {
      key => left => right =>

        log(key, left, right, None)
        Left(mkError(s"Found unexpected duplicate values" +
          s" for key $key: ($left, $right) "))
    }

    private def preferSide(
      chooseF: ValueType => ValueType => ValueType):
      HandleDuplicatesF[ErrorType] = {
      key => left => right =>
        val choice = chooseF(left)(right)
        log(key, left, right, Some(choice))
        Right((key, choice))
    }

    private def chooseLeft:  ValueType => ValueType => ValueType = x => y => x
    private def chooseRight: ValueType => ValueType => ValueType = x => y => y

    def preferLeft:  HandleDuplicatesF[ErrorType] = preferSide(chooseLeft)
    def preferRight: HandleDuplicatesF[ErrorType] = preferSide(chooseRight)
  }

  object HandleDuplicatesFunctions {
    val defaultEnableLogging: Boolean = true
  }

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
