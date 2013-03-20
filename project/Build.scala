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

  lazy val lenser = Project(
    id = "lenser",
    base = file("lenser")
  ).settings(
    baseSettings ++ seq(
      libraryDependencies ++= Seq(
        "com.github.hexx" %% "lenser-macro" % "0.0.1",
        "org.scalaz" %% "scalaz-core" % "7.0.0-M9"
      )
    ) : _*
  ).dependsOn(macro)

  lazy val argonaut = Project(
    id = "argonaut",
    base = file("argonaut")
  ).settings(
    baseSettings ++ seq(
      name := "argonaut",
      version := "0.0.1",
      libraryDependencies ++= Seq(
        "com.github.hexx" %% "lenser-macro" % "0.0.1",
        "io.argonaut" %% "argonaut" % "6.0-M3",
        "com.chuusai" %% "shapeless" % "1.2.4"
      ),
      initialCommands in console += Seq(
        "scalaz._",
        "Scalaz._",
        "argonaut._",
        "Argonaut._",
        "shapeless._",
        "com.github.hexx.argonaut._"
      ).map("import " + _ + "\n").mkString,
      initialCommands in console += "case class Address(name: String)\n",
      initialCommands in console += "case class Person(name: String, age: Int, address: Address)\n",
      initialCommands in console += """val p = Person("HogeIka", 13, Address("Funabashi"))""" + "\n",
      initialCommands in console += """val pj = "{\"address\":{\"name\":\"Shibuya\"},\"age\":\"29\",\"name\":\"kmizu\"}""""
    ) : _*
  ).dependsOn(macro)
}
