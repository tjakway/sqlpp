package com.jakway.sqlpp.util

import java.util.Properties

import scala.util.Try

object MapToProperties {
  /**
   * returns a map of overwritten values (i.e. the key-value pairs
   * where the key was already present in the map before this method call)
   * @param kvs
   * @param prop
   * @return
   */
  def apply(kvs: Map[String, String])
           (prop: Properties): Try[Map[String, Object]] = Try {

    val empty: Map[String, Object] = Map()
    kvs.foldLeft(empty) {
      case (prevValues, (key, value)) => {
        Option(prop.setProperty(key, value)) match {
          case Some(oldValue) => prevValues.updated(key, oldValue)
          case None => prevValues
        }
      }
    }
  }
}
