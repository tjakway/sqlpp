import sqlpp.project.CommonSettings
import sqlpp.project.Dependencies

name := "exe"

version := CommonSettings.version

scalaVersion := CommonSettings.scalaVersion

scalacOptions ++= CommonSettings.scalacOptions

val main = Some("com.jakway.sqlpp.Main")

Compile / mainClass := main 

//sbt-assembly settings
//see https://github.com/sbt/sbt-assembly
assembly / mainClass := main
assembly / assemblyJarName := "sqlpp.jar"
