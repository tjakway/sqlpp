import sqlpp.project.CommonSettings
import sqlpp.project.Dependencies

name := "sqlpp"

version := CommonSettings.version

scalaVersion := CommonSettings.scalaVersion

scalacOptions ++= CommonSettings.scalacOptions

libraryDependencies ++= Dependencies.all

//scalatest recommends unbuffered test output 
//see http://www.scalatest.org/user_guide/using_scalatest_with_sbt
Test / logBuffered := false

//make ScalaCheck give stack traces
//see https://stackoverflow.com/questions/24396407/how-to-display-entire-stack-trace-for-thrown-exceptions-from-scalacheck-tests
Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")
