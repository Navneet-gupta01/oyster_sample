ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.navneetgupta"
ThisBuild / organizationName := "navneetgupta"
ThisBuild / name             := "oyster"


scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-Xfatal-warnings",
  "-Ypartial-unification",
  "-language:higherKinds",
  "-language:implicitConversions"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M11" cross CrossVersion.full)

libraryDependencies ++= {
  val CatVersion = "2.0.0-M1"
  val ZioVersion = "1.0-RC4"
  Seq(

    "org.scalactic"   %% "scalactic"                % "3.0.5",
    "org.scalatest"   %% "scalatest"                % "3.0.5" % "test",
    "org.typelevel"   %% "cats-core"                % CatVersion,
    "org.scalaz"      %% "scalaz-zio"               % ZioVersion,
    "org.scalaz"      %% "scalaz-zio-interop-cats"  % ZioVersion,
  )
}
