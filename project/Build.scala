import sbt._
import Keys._

object Build extends sbt.Build {
  lazy val baseSettings = seq(
    scalaVersion := "2.10.1",
    organization := "com.github.hexx",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")
  )

  lazy val macro = Project(
    id = "macro",
    base = file("macro")
  ).settings(
    baseSettings ++ seq(
      name := "lenser-macro",
      version := "0.0.1",
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)
    ) : _*
  )

  lazy val argonaut = Project(
    id = "argonaut",
    base = file("argonaut")
  ).settings(
    baseSettings ++ seq(
      name := "lenser-argonaut",
      version := "0.0.1",
      libraryDependencies ++= Seq(
        "com.github.hexx" %% "lenser-macro" % "0.0.1",
        "io.argonaut" %% "argonaut" % "6.0-M2"
      ),
      initialCommands in console += Seq(
        "scalaz._",
        "Scalaz._",
        "argonaut._",
        "Argonaut._",
        "com.github.hexx.macros.lenser._",
        "com.github.hexx.macros.argonaut._",
        "com.github.hexx.argonaut._"
      ).map("import " + _ + "\n").mkString,
      initialCommands in console += "case class Address(name: String)\n",
      initialCommands in console += "case class Person(name: String, age: Int, address: Address)\n",
      initialCommands in console += """val p = Person("HogeIka", 13, Address("Funabashi"))""" + "\n"
    ) : _*
  ).dependsOn(macro)
}
