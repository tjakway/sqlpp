package com.jakway.sqlpp.template

import com.jakway.sqlpp.error.SqlppError
import com.jakway.sqlpp.util.{ContextUtil, MapToProperties}
import org.apache.velocity.VelocityContext
import java.util.Properties

import scala.util.{Failure, Success, Try}

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
    import java.util.{Map => JMap}
    import scala.collection.JavaConverters

    val convMap: JMap[String, Object] = {
      val objMap = map.map {
        case (key, value) => (key, value: Object)
      }

      JavaConverters.mapAsJavaMap(objMap)
    }

    Try(new VelocityContext(convMap)) match {
      case Success(x) => Right(x)
      case Failure(t) => Left(ValueSource.ValueSourceError(t))
    }
  }
}

case class VelocityContextSource(vc: VelocityContext) extends ValueSource {
  override def toVelocityContext: Either[SqlppError, VelocityContext] =
    Right(vc)
}
