package com.jakway.sqlpp.template

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.Backend.Lookup.Error.TooManyMatches
import com.jakway.sqlpp.template.Backend.NamelessBackendError

/**
 * @param names
 * @param templateIdentifier resource path, file path, etc.
 *                           Whatever is recognized by the resource loader
 */
case class Backend(names: Set[String],
                   templateIdentifier: String) {
  def matches(x: String): Boolean =
    names.exists(Backend.areEqual(_, x))

  def toOutputStringName: Either[SqlppError, String] =
    names.headOption match {
      case Some(x) => Right(x)
      case None => Left(new NamelessBackendError(
        s"Backend with template identifier $templateIdentifier" +
          s" has no name"
      ))
    }
}

object Backend {
  class NamelessBackendError(override val msg: String)
    extends SqlppError(msg)

  def areEqual(left: String, right: String): Boolean = {
    def normalizeString(str: String): String = str.trim.toLowerCase()

    normalizeString(left) == normalizeString(right)
  }

  object Lookup {
    type LookupF[A] = Set[Backend] => String => Either[SqlppError, A]

    object Error {
      class BackendLookupError(override val msg: String)
        extends SqlppError(msg)

      class TooManyMatches(val matchingBackends: Set[Backend],
                           val name: String)
        extends BackendLookupError(s"Expected at most 1 backend to match" +
          s" passed name $name but all of $matchingBackends match")
    }

    def getAllMatches: LookupF[Set[Backend]] = { backends => name =>
      val empty: Set[Backend] = Set.empty
      Right apply backends.foldLeft(empty) {
        case (acc, thisBackend) => {
          if(thisBackend.matches(name)) {
            acc + thisBackend
          } else {
            acc
          }
        }
      }
    }

    def findMatchWithoutOverlap: LookupF[Option[Backend]] = { backends => name =>
      getAllMatches(backends)(name).flatMap { matches =>
        if(matches.size == 1) {
          Right(matches.headOption)
        } else {
          Left(new TooManyMatches(matches, name))
        }
      }
    }
  }
}