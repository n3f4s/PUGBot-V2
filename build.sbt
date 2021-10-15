val scala3Version = "3.0.2" // FIXME use 3.0.0 for metals?
// val scala3Version = "3.0.0" // FIXME use 3.0.0 for metals?

val scala2deps = Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.8",
  "com.typesafe.akka" %% "akka-stream" % "2.6.8",
  "com.typesafe.akka" %% "akka-http" % "10.2.0",
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.8",
  "com.typesafe.akka" %% "akka-stream-typed" % "2.5.32",

  "org.scalaz" %% "scalaz-core" % "7.2.29",

  "io.projectreactor" %% "reactor-scala-extensions" % "0.8.+",

  "org.json4s" %% "json4s-jackson" % "3.+",

  "com.lihaoyi" %% "requests" % "0.2.0",

).map(d ⇒ d.cross(CrossVersion.for3Use2_13))

val circeVersion = "0.14.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "PUGBot",
    version := "0.1.0",

    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-language:implicitConversions",
      ),


    scalaVersion := scala3Version,

    resolvers += "d4j-snapshot" at "https://oss.sonatype.org/content/repositories/snapshots",

    libraryDependencies ++= Seq(
      // "com.discord4j" % "discord4j-core" % "3.0.+",
      "com.discord4j" % "discord4j-core" % "3.2.0-SNAPSHOT",
      "com.discord4j" % "discord-json" % "1.6.11-SNAPSHOT",
      "org.immutables" % "value" % "2.8.8",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.hubspot.jinjava" % "jinjava" % "2.5.9",
      // ( "com.discord4j" % "discord4j-core" % "3.2.0-SNAPSHOT" ).cross(CrossVersion.for3Use2_13),

    ) ++ scala2deps ++ Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser",
    ).map( _ % circeVersion),

    assembly / assemblyJarName := "herald.jar",
    assembly / assemblyOutputPath := file("./herald.jar"),

    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case PathList("reference.conf") => MergeStrategy.concat
      case x => MergeStrategy.first
    }


  )
