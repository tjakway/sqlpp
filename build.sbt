lazy val root = (project in file("."))
    .aggregate(common, main, build_templates)

lazy val main = (project in file("main"))
                    .dependsOn(common)
lazy val common = (project in file("common"))

lazy val build_templates = (project in file("build_templates"))
                    .dependsOn(common)
