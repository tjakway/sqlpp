import sbt._

package sqlpp.project {
  object Dependencies {

    val common = 
      Seq("org.slf4j" % "slf4j-parent" % "1.7.6",
          "ch.qos.logback"  %  "logback-classic"    % "1.2.1",
          "com.github.scopt" %% "scopt" % "3.7.1")

    val testing = Seq(
        //scalatest
        //see http://www.scalatest.org/install
        "org.scalatest" % "scalatest-flatspec_2.12" % "3.2.0-M2" % "test",
        "org.scalatest" % "scalatest-shouldmatchers_2.12" % "3.2.0-M2" % "test",
        "org.scalatest" % "scalatest-propspec_2.12" % "3.2.0-M2" % "test",
        "org.scalactic" % "scalactic_2.12" % "3.2.0-M2" % "test",

        "org.scalacheck" %% "scalacheck" % "1.14.2" % "test")

    val apacheVelocity = Seq(
      "org.apache.velocity" % "velocity-engine-core" % "2.1")

    val all = common ++ testing ++ apacheVelocity
  }
}
