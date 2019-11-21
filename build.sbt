lazy val root = (project in file("."))
    .aggregate(common, main)

lazy val main = (project in file("main"))
lazy val common = (project in file("common"))
