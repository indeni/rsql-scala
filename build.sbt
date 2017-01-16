organization := "com.indeni"
name := """rsql-scala"""
version := "1.0"
licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

pomExtra in ThisBuild := (
  <url>https://github.com/indeni/rsql-scala</url>
    <licenses>
      <license>
        <name>Apache</name>
        <url>http://www.opensource.org/licenses/Apache-2.0</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:indeni/rsql-scala</url>
      <connection>scm:git:git@github.com:indeni/rsql-scala.git</connection>
    </scm>
    <developers>
      <developer>
        <id>yarinbenado</id>
        <name>Yarin Benado</name>
        <email>yarin@indeni.com</email>
        <organization>indeni</organization>
      </developer>
    </developers>
  )

val unusedWarnings = "-Ywarn-unused" :: "-Ywarn-unused-import" :: Nil
scalacOptions ++= {
  Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions", "-language:higherKinds", "-Xfuture", "-Xlint")
}

scalaVersion := "2.11.8"
// sbt "release cross"
crossScalaVersions := Seq("2.11.8", "2.12.1")
scalacOptions ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
  case Some((2, scalaMajor)) if scalaMajor >= 11 => unusedWarnings
}.toList.flatten



libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"
)
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
