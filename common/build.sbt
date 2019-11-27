import sqlpp.project.CommonSettings
import sqlpp.project.Dependencies

name := "common"

version := CommonSettings.version

scalaVersion := CommonSettings.scalaVersion

scalacOptions ++= CommonSettings.scalacOptions

libraryDependencies ++= (Dependencies.common ++ Dependencies.apacheVelocity)
