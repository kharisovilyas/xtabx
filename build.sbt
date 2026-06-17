val scala3Version = "3.3.4"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "xml-to-adt-by-xsd",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml"            % "2.3.0",
      "co.fs2"                 %% "fs2-core"             % "3.11.0",
      "org.typelevel"          %% "cats-effect"          % "3.5.4",
      "org.gnieh"              %% "fs2-data-xml"         % "1.11.2",
      "co.fs2"                 %% "fs2-io"               % "3.11.0" % Test,
      "org.scalameta"          %% "munit"                % "1.0.2"  % Test,
      "org.typelevel"          %% "munit-cats-effect"    % "2.0.0"  % Test
    ),
    testFrameworks += new TestFramework("munit.Framework"),

    Compile / sourceGenerators += Def.task {
      XsdDescriptorGen.generate(
        (Compile / sourceManaged).value,
        baseDirectory.value / "edo_test_schema.xsd"
      )
    }.taskValue
  )
