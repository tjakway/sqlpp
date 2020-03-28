package com.jakway.sqlpp.config.test.util

import java.util.Formatter

import org.slf4j.Logger

object LogEither {
  def logOnLeft: Logger => String => Unit =
    logger => msg => logger.error(msg)

  def logOnRight: Logger => String => Unit =
    logger => msg => logger.debug(msg)

  val defaultFormatString: String = "%s"

  def apply[L, R](logger: Logger,
                  e: Either[L, R],
                  formatString: String = defaultFormatString): Unit = {
    val msg = {
      val sb: StringBuffer = new StringBuffer()
      val fmt: Formatter = new Formatter(sb)
      fmt.format(formatString, e.toString)
      fmt.toString
    }

    e match {
      case Right(_) => logOnRight(logger)(msg)
      case Left(_) => logOnLeft(logger)(msg)
    }
  }
}
