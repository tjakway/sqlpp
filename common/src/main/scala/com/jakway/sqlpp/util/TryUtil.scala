package com.jakway.sqlpp.util

import java.util.Formatter

import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

object TryUtil {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def alwaysDoAfter[A](first: Try[A],
                       second: => Try[Unit],
                       msg: String = ""): Try[A] = {
    val firstRes = first

    second match {
      case Success(_) => firstRes
      case Failure(t) => {
        val fmt: Formatter = {
          val sb = new StringBuffer()
          new Formatter(sb)
        }
        fmt.format("Error in TryUtil.alwaysDoAfter")
        if(msg.nonEmpty) {
          fmt.format(": %s", msg)
        }

        logger.error(fmt.toString, t)

        firstRes
      }
    }
  }

}
