val scala3Version = "3.3.4"

addCommandAlias("start", "runMain edo.Main")

lazy val root = project
  .in(file("."))
  .settings(
    name         := "xtabx",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml"            % "2.3.0",
      "co.fs2"                 %% "fs2-core"             % "3.11.0",
      "org.typelevel"          %% "cats-effect"          % "3.5.4",
      "org.gnieh"              %% "fs2-data-xml"         % "1.11.2",
      "co.fs2"                 %% "fs2-io"               % "3.11.0",
      "org.scalameta"          %% "munit"                % "1.0.2"  % Test,
      "org.typelevel"          %% "munit-cats-effect"    % "2.0.0"  % Test
    ),
    testFrameworks += new TestFramework("munit.Framework"),
    run / fork := true,
    run / javaOptions += "-Dfile.encoding=UTF-8",
    run / outputStrategy := Some(StdoutOutput),

    Compile / sourceGenerators += Def.task {
      val schemaDir = baseDirectory.value / "schema"
      val xsdFiles  = schemaDir.listFiles().toList.filter(_.getName.endsWith(".xsd"))
      xsdFiles.flatMap { xsd =>
        XsdDescriptorGen.generate((Compile / sourceManaged).value, xsd)
      }
    }.taskValue
  )
