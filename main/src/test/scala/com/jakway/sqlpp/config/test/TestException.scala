package com.jakway.sqlpp.config.test

class TestException(val msg: String,
                    val cause: Option[Throwable] = None)
  //null indicates no or unknown cause,
  //see https://docs.oracle.com/javase/7/docs/api/java/lang/RuntimeException.html#RuntimeException(java.lang.String,%20java.lang.Throwable)
  extends RuntimeException(msg, cause.orNull)

