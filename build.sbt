
/*
 build.sbt adapted from https://github.com/pbassiner/sbt-multi-project-example/blob/master/build.sbt
*/

name := "bwhc-hgnc-catalog"
organization in ThisBuild := "de.bwhc"
version in ThisBuild:= "1.0-SNAPSHOT"


lazy val scala212 = "2.12.10"
lazy val scala213 = "2.13.8"
lazy val supportedScalaVersions =
  List(
    scala212,
    scala213
  )

scalaVersion in ThisBuild := scala213

//-----------------------------------------------------------------------------
// PROJECTS
//-----------------------------------------------------------------------------

lazy val global = project
  .in(file("."))
  .settings(
    settings,
    crossScalaVersions := Nil,
    publish / skip := true
  )
  .aggregate(
     api,
     impl,
     tests
  )



lazy val api = project
  .settings(
    name := "hgnc-api",
    settings,
    libraryDependencies ++= Seq(
      dependencies.cats_core,
      dependencies.play_json
    ),
    crossScalaVersions := supportedScalaVersions
  )

lazy val impl = project
  .settings(
    name := "hgnc-impl",
    settings,
    libraryDependencies ++= Seq(
      dependencies.slf4j
    ),
    crossScalaVersions := supportedScalaVersions
  )
  .dependsOn(api)

lazy val tests = project
  .settings(
    name := "tests",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest
    ),
    crossScalaVersions := supportedScalaVersions,
    publish / skip := true
  )
  .dependsOn(
    api,
    impl % "test"
  )



//-----------------------------------------------------------------------------
// DEPENDENCIES
//-----------------------------------------------------------------------------

lazy val dependencies =
  new {
    val scalatest  = "org.scalatest"     %% "scalatest"        % "3.1.1" % Test
    val slf4j      = "org.slf4j"         %  "slf4j-api"        % "1.7.32"
    val cats_core  = "org.typelevel"     %% "cats-core"        % "2.1.1"
    val play_json  = "com.typesafe.play" %% "play-json"        % "2.8.1"
  }

lazy val commonDependencies = Seq(
  dependencies.slf4j,
  dependencies.scalatest,
)


//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings


lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
//  "-language:existentials",
//  "-language:higherKinds",
//  "-language:implicitConversions",
//  "-language:postfixOps",
  "-deprecation",
  "-Xfatal-warnings",
  "-encoding", "utf8"
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  )
)

