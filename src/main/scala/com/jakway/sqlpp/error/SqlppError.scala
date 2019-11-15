package com.jakway.sqlpp.error

class SqlppError(val msg: String)
  extends RuntimeException(msg)
