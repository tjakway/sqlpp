package com.jakway.sqlpp.util

import scala.util.{Try, Success, Failure}

object TryToEither {
  def apply[L, R](handleFailure: Throwable => L)
                 (t: => Try[R]): Either[L, R] = {
    t match {
      case Success(x) => Right(x)
      case Failure(t) => Left(handleFailure(t))
    }
  }
}
