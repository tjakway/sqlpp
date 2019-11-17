name := "sqlpp"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions ++= Seq("-feature")

libraryDependencies ++= 
  Seq("org.slf4j" % "slf4j-parent" % "1.7.6",
      "ch.qos.logback"  %  "logback-classic"    % "1.2.1",
      "com.github.scopt" %% "scopt" % "3.7.1",

      //scalatest
      //see http://www.scalatest.org/install
      "org.scalatest" %% "scalatest" % "3.0.4" % "test",
      "org.scalactic" %% "scalactic" % "3.0.4" % "test",

      "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",

      //json parser
      "org.json4s" % "json4s-native_2.12" % "3.6.7"
      )

//enable more warnings
scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

//scalatest recommends unbuffered test output 
//see http://www.scalatest.org/user_guide/using_scalatest_with_sbt
logBuffered in Test := false

//make ScalaCheck give stack traces
//see https://stackoverflow.com/questions/24396407/how-to-display-entire-stack-trace-for-thrown-exceptions-from-scalacheck-tests
testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")
