package com.jakway.sqlpp

import com.jakway.sqlpp.Run.Result

object Main {
  def main(args: Array[String]): Unit = {
    Run.apply(args) match {
        //print any error messages to stderr and exit
      case Result(exitCode, errorMessage) => {
        errorMessage.foreach(System.err.print)
        System.exit(exitCode)
      }
    }
  }
}