package com.jakway.sqlpp.config.unchecked

import scopt.DefaultOParserSetup

class ParserSetup extends DefaultOParserSetup {
  //StringBuffer is thread-safe, StringBuilder is not
  private val warnings: StringBuffer = new StringBuffer()
  private val errors: StringBuffer = new StringBuffer()

  private def opt(s: String): Option[String] = {
    val x = s.trim
    if(x.isEmpty) {
      None
    } else {
      Some(x)
    }
  }

  def getWarnings: Option[String] = opt(warnings.toString)
  def getErrors: Option[String] = opt(errors.toString)


  override def terminate(exitState: Either[String, Unit]): Unit =
    exitState match {
      case Left(errMsg) => errors.append(errMsg)
      case _ => {}
  }


  override def reportError(msg: String): Unit = errors.append(msg)
  override def reportWarning(msg: String): Unit = warnings.append(msg)
}
