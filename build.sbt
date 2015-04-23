scalaVersion := "2.11.6"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scala-lang"     % "scala-reflect" % scalaVersion.value,
  "com.chuusai"       %% "shapeless"     % "2.2.0-RC4",
  "com.typesafe.play" %% "play-json"     % "2.3.4"
)

initialCommands in console := """
  |import shapeless._
  |import play.api.libs.json._
  |import Main._
""".trim.stripMargin
