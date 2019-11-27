lazy val root = (project in file("."))
    .aggregate(common, main)

lazy val main = (project in file("main"))
                    .dependsOn(common)
lazy val common = (project in file("common"))
