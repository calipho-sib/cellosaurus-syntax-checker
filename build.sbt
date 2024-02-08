val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "csc",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
//    Compile / mainClass := Some("org.nextprot.parser.cellosaurus.DoublingTimeStateAutomaton"),
//    Compile / mainClass := Some("org.nextprot.parser.cellosaurus.ParserPlayground"),
//    Compile / mainClass := Some("org.nextprot.parser.cellosaurus.SourceChecker"),
//    Compile / mainClass := Some("org.nextprot.parser.cellosaurus.Utf8Checker"),
    Compile / mainClass := Some("org.nextprot.parser.cellosaurus.CelloParser"),
    scalacOptions ++= Seq("-deprecation", "-explain"),
    exportJars := true, 
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.scala-lang.modules" % "scala-xml_3" % "2.2.0"
  )



