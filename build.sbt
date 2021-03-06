lazy val root = (project in file("."))
    .aggregate(common, main, build_templates, exe)
    .settings(testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"))
    .settings(parallelExecution in Test := false)

lazy val main = (project in file("main"))
                    .dependsOn(common)
lazy val common = (project in file("common"))

lazy val build_templates = (project in file("build_templates"))
                    .dependsOn(common)

lazy val exe = (project in file("exe"))
                    .dependsOn(main)
