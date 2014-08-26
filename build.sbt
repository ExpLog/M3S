name := "M3S"

version := "1.1"

scalaVersion := "2.11.1"

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits", "-diagrams", "-feature")

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
	"org.scalatest" % "scalatest_2.11" % "2.1.6" % "test" withJavadoc(),
	"org.scalacheck" %% "scalacheck" % "1.11.4" % "test" withJavadoc(),
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2" withJavadoc() withSources()
)