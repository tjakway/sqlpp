package com.jakway.sqlpp

import com.jakway.sqlpp.error.SqlppError

import com.jakway.sqlpp.util.{ContextUtil, MapToProperties}

import org.apache.velocity.VelocityContext
import java.util.Properties


import scala.util.{Success, Failure}

sealed trait ValueSource {
  def toVelocityContext: Either[SqlppError, VelocityContext]

  //TODO: implement using the VelocityContext(Context innerContext)
  //constructor
  //def merge(other: ValueSource): Either[SqlppError, ValueSource]
}

object ValueSource {
  class ValueSourceError(override val msg: String)
    extends SqlppError(msg)

  object ValueSourceError {
    def apply(t: Throwable): ValueSourceError = 
      new ValueSourceError("Error caused by throwable: " + 
        SqlppError.formatThrowable(t))
  }
}

case class PropertySource(prop: Properties) extends ValueSource {
  override def toVelocityContext: Either[SqlppError, VelocityContext] = {
    ContextUtil.propertiesToContextE_(prop)
  }
}

case class MapSource(map: Map[String, String]) extends ValueSource {
  override def toVelocityContext: Either[SqlppError, VelocityContext] = {
    MapToProperties.withNewProperties(map) match {
      case Success(x) => Right(x)
      case Failure(t) => Left(ValueSource.ValueSourceError(t))
    }
  }
}

case class VelocityContextSource(vc: VelocityContext) extends ValueSource {
  override def toVelocityContext: Either[SqlppError, VelocityContext] =
    Right(vc)
}
