package com.jakway.sqlpp.config.test

object TestResources {
  val testBackendsDir: String = "test_backends"

  val h2Backend: String = inTestBackendsDir("h2.xml")
  val postgresBackend: String = inTestBackendsDir("postgres.xml")
  val defaultBackend: String = inTestBackendsDir("defaults.xml")

  private def subdir(inDir: String, item: String): String =
    inDir + "/" + item
  private def inTestBackendsDir(item: String): String = subdir(testBackendsDir, item)

}
