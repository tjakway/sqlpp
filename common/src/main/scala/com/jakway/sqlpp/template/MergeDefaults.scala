package com.jakway.sqlpp.template

import java.io.File

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.util.MergeProperties
import org.slf4j.{Logger, LoggerFactory}

class MergeDefaults(val defaultsFilename: String
                      = MergeDefaults.defaultsFilename) {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  class MergeDefaultsError(override val msg: String)
    extends SqlppError(msg)

  object MergeDefaultsError {
    def apply(throwable: Throwable): MergeDefaultsError =
      new MergeDefaultsError(SqlppError.formatThrowable(throwable))
  }


  private def preferRightSide: MergeProperties.HandleDuplicatesF[SqlppError] = {
    key => left => right =>
      logger.debug(s"Duplicate properties for $key:" +
        s" ($left, $right), preferring right side")

      Right((key, right))
  }

  private def noDuplicates: MergeProperties.HandleDuplicatesF[SqlppError] = {
    key => left => right =>
      Left(new MergeDefaultsError(
        s"Unexpected duplicate properties for $key:" +
          s" ($left, $right)"))
  }

  def mergeDefaults(propertySources: Seq[File]):
  Either[SqlppError, Seq[PropertySource]] = {

    val eVSMap: Either[SqlppError, Map[File, PropertySource]] =
      propertySources.foldLeft(
        Right(Map.empty): Either[SqlppError, Map[File, PropertySource]]) {
        case (eAcc, thisSourceFile) => eAcc.flatMap { acc =>
          PropertySource.fromXML(thisSourceFile)
            .map(acc.updated(thisSourceFile, _))
        }
      }

    eVSMap.flatMap { vsMap =>
      val empty: (Option[(PropertySource, Int)], Map[File, PropertySource], Int) =
        (None, Map.empty, 0)

      //split the list of properties into defaults and others
      //
      //alternatively we could just find the defaults while
      //leaving them in the list while making sure to properly
      //merge duplicates that are equal
      val (defaultsOption, rest, _) = vsMap.foldLeft(empty) {
        case ((None, acc, pos), (thisSourceFile, prop)) => {
          if(thisSourceFile.getName == defaultsFilename) {
            (Some((prop, pos)), acc, pos + 1)
          } else {
            (None, acc.updated(thisSourceFile, prop), pos + 1)
          }
        }
        case ((x@Some(_), acc, pos), (thisSourceFile, prop)) => {
          (x, acc.updated(thisSourceFile, prop), pos + 1)
        }
      }

      defaultsOption match {
        case Some((defaults, defaultsPos)) => {
          val res = {
            rest.foldLeft(
              Right(Seq.empty): Either[SqlppError, Seq[PropertySource]]) {
              case (eAcc, (thisSourceFile, thisSource)) => eAcc.flatMap { acc =>
                val eMergedProperties = MergeProperties.merge(
                  defaults.prop, thisSource.prop,
                  preferRightSide,
                  MergeDefaultsError.apply)

                eMergedProperties.map { mergedProperties =>
                  acc :+ PropertySource(mergedProperties)
                }
              }
            }
          }

          //add defaults back to the list at the position it was found
          res.map { acc =>
            val (left, right) = acc.splitAt(defaultsPos)
            left ++ Seq(defaults) ++ right
          }
        }
        case None => Left(new MergeDefaultsError(
          s"Expected to find a template named $defaultsFilename " +
            s"containing default properties"))
      }

    }
  }
}

object MergeDefaults {
  val defaultsFilename: String = "defaults.xml"
}
