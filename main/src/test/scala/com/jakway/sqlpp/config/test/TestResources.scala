package com.jakway.sqlpp.config.test

import com.jakway.sqlpp.config.test.error.TestError
import com.jakway.sqlpp.error.SqlppError

object TestResources {
  val testBackendsDir: String = root("test_backends")

  val h2Backend: String = inTestBackendsDir("h2.xml")
  val postgresBackend: String = inTestBackendsDir("postgres.xml")
  val defaultsBackend: String = inTestBackendsDir("defaults.xml")

  private def root(x: String): String = "/" + x

  private def subdir(inDir: String, item: String): String =
    inDir + "/" + item
  private def inTestBackendsDir(item: String): String = subdir(testBackendsDir, item)


  object Tests {
    private val testsDir: String = root("tests")
    private def apply(testName: String): String = subdir(testsDir, testName)

    val simple: String = apply("simple_test.xml")
    val empty: String = apply("empty.xml")
    val allVariables: String = apply("all_variables.xml")
  }

  class OpenTestResourceError(override val msg: String)
    extends TestError(msg) {
    def this(t: Throwable) {
      this(SqlppError.formatThrowableCause(t))
    }
  }
}
