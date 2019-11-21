import sbt._

package sqlpp.project {
  object CommonSettings {
    val version = "0.1"
    val scalaVersion = "2.12.10"
    val scalacOptions = Seq("-unchecked", "-deprecation", "-feature")
  }
}
