package com.jakway.sqlpp.util

import java.util.Properties

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.util.MergeMaps.HandleDuplicatesF

import org.apache.velocity.VelocityContext

import scala.util.{Failure, Success, Try}

object ContextUtil {
  class PropertiesToContextError(override val msg: String)
    extends SqlppError(msg)

  object PropertiesToContextError {
    def apply(t: Throwable): PropertiesToContextError = {
      new PropertiesToContextError("Error caused by throwable: " +
        SqlppError.formatThrowable(t))
    }
  }

  def propertiesToContext(properties: Properties): Try[VelocityContext] = {
    MergeProperties
      .propertiesToMap(properties)
      .flatMap { propMap => Try {
        val vc = new VelocityContext()
        propMap.foreach {
          case (thisKey, thisValue) => 
            vc.put(thisKey, thisValue)
          }
        vc
        }
      }
  }

  def propertiesToContextE[ErrorType](properties: Properties,
                                      handleError: Throwable => ErrorType):
    Either[ErrorType, VelocityContext] = {
      
    propertiesToContext(properties) match {
      case Success(x) => Right(x)
      case Failure(t) => Left(handleError(t))
    }
  }

  def propertiesToContextE_(properties: Properties): 
    Either[SqlppError, VelocityContext] =
    propertiesToContextE_(properties, PropertiesToContextError.apply)
}
