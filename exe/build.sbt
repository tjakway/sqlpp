import sqlpp.project.CommonSettings
import sqlpp.project.Dependencies

name := "exe"

version := CommonSettings.version

scalaVersion := CommonSettings.scalaVersion

scalacOptions ++= CommonSettings.scalacOptions

val main = Some("com.jakway.sqlpp.Main")

mainClass in (Compile, run) := main 

//sbt-assembly settings
//see https://github.com/sbt/sbt-assembly
mainClass in assembly := main
assemblyJarName in assembly := "sqlpp.jar"
