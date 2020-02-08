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

  //unfortunately can't overload on the type of Try[R]
  //would be possible in C++
  def handleNested[L, R](handleFailure: Throwable => L)
                        (t: => Try[Either[L, R]]): Either[L, R] = {

    //unwrap and return the inner Either
    apply(handleFailure)(t) match {
      case Right(x) => x
      case Left(e) => Left(e)
    }
  }
}

