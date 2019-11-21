import sqlpp.project.CommonSettings
import sqlpp.project.Dependencies

name := "main"

version := CommonSettings.version

scalaVersion := CommonSettings.scalaVersion

scalacOptions ++= CommonSettings.scalacOptions

libraryDependencies ++= Dependencies.all

//scalatest recommends unbuffered test output 
//see http://www.scalatest.org/user_guide/using_scalatest_with_sbt
logBuffered in Test := false

//make ScalaCheck give stack traces
//see https://stackoverflow.com/questions/24396407/how-to-display-entire-stack-trace-for-thrown-exceptions-from-scalacheck-tests
testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")
