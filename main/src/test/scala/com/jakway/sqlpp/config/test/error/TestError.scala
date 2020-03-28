package com.jakway.sqlpp.config.test.error

import com.jakway.sqlpp.error.SqlppError

class TestError(override val msg: String) extends SqlppError(msg)
