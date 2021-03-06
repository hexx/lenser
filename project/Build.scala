import sbt._
import Keys._

object Build extends sbt.Build {
  lazy val baseSettings = seq(
    scalaVersion := "2.10.1",
    organization := "com.github.hexx",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")
  )

  lazy val exampleSettings = seq(
    initialCommands in console += "case class Address(name: String)\n",
    initialCommands in console += "case class Person(name: String, age: Int, address: Address)\n",
    initialCommands in console += """val a = Address("Akihabara")""" + "\n",
    initialCommands in console += """val p = Person("HogeIka", 13, Address("Funabashi"))""" + "\n"
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
    baseSettings ++ exampleSettings ++ seq(
      libraryDependencies ++= Seq(
        "com.github.hexx" %% "lenser-macro" % "0.0.1",
        "org.scalaz" %% "scalaz-core" % "7.0.0-RC1"
      ),
      initialCommands in console += Seq(
        "scalaz._",
        "Scalaz._",
        "com.github.hexx.macros._",
        "com.github.hexx.scalaz._"
      ).map("import " + _ + "\n").mkString
    ) : _*
  ).dependsOn(macro)

  lazy val argonaut = Project(
    id = "argonaut",
    base = file("argonaut")
  ).settings(
    baseSettings ++ exampleSettings ++ seq(
      name := "argonaut",
      version := "0.0.1",
      libraryDependencies ++= Seq(
        "com.github.hexx" %% "lenser-macro" % "0.0.1",
        "io.argonaut" %% "argonaut" % "6.0-M4"
      ),
      initialCommands in console += Seq(
        "scalaz._",
        "Scalaz._",
        "argonaut._",
        "Argonaut._",
        "com.github.hexx.macros._",
        "com.github.hexx.argonaut._"
      ).map("import " + _ + "\n").mkString,
      initialCommands in console += """val pj = "{\"address\":{\"name\":\"Shibuya\"},\"age\":\"29\",\"name\":\"kmizu\"}""""
    ) : _*
  ).dependsOn(macro)
}
