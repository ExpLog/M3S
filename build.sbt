name := "M3S"

version := "1.1"

scalaVersion := "2.11.1"

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits", "-diagrams", "-feature")