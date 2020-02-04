package com.jakway.sqlpp.template.backend

import java.io.File
import java.util.Properties

import com.jakway.sqlpp.config.Constants
import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.template.DelegatingValueSource
import com.jakway.sqlpp.template.backend.Backend.Lookup.Error.WrongNumberOfMatches
import com.jakway.sqlpp.template.backend.Backend.NamelessBackendError

import scala.util.Try

/**
 * @param names
 */
abstract class Backend(val names: Set[String])
  extends DelegatingValueSource {
  def matches(x: String): Boolean =
    names.exists(Backend.areEqual(_, x))

  def getName: Either[SqlppError, String] =
    names.headOption match {
      case Some(x) => Right(x)
      case None => Left(new NamelessBackendError(
          toString + " has no name"))
    }
}

object Backend {
  class NamelessBackendError(override val msg: String)
    extends SqlppError(msg)

  class Data(val backendProperties: Properties)

  def areEqual(left: String, right: String): Boolean = {
    def normalizeString(str: String): String = str.trim.toLowerCase()

    normalizeString(left) == normalizeString(right)
  }

  object Lookup {
    type LookupF[A] = Set[Backend] => String => Either[SqlppError, A]

    object Error {
      class BackendLookupError(override val msg: String)
        extends SqlppError(msg)

      class WrongNumberOfMatches(val matchingBackends: Set[Backend],
                                 val name: String)
        extends BackendLookupError(s"Expected at most 1 backend to match" +
          s" passed name $name but < $matchingBackends > match")
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
          Left(new WrongNumberOfMatches(matches, name))
        }
      }
    }
  }
}