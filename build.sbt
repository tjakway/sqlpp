lazy val root = (project in file("."))
    .aggregate(common, main, build_templates, exe)
    .settings(Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"))
    .settings(Test / parallelExecution := false)

lazy val main = (project in file("main"))
                    .dependsOn(common)
lazy val common = (project in file("common"))

lazy val build_templates = (project in file("build_templates"))
                    .dependsOn(common)

lazy val exe = (project in file("exe"))
                    .dependsOn(main)
