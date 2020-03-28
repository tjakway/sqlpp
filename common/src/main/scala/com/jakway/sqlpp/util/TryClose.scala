package com.jakway.sqlpp.util

import java.io.Closeable

import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

object TryClose {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def apply(c: Closeable, description: Option[String]): Unit = {
    Try(c.close()) match {
      case Success(_) => {}
      case Failure(t) => synchronized {
        val desc: String = description.getOrElse(c.toString)
        logger.warn(s"Failed to close $desc, threw $t")
      }
    }
  }
}
