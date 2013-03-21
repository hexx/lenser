package com.github.hexx.scalaz

import scalaz._, Scalaz._

import com.github.hexx.macros.{Lenser => MacroLenser, Lens => MacroLens}

class Lenser[A] {
  def lens[B](f: MacroLenser[A] => MacroLens[A, B]) = {
    val l = f(new MacroLenser[A])
    Lens.lensu(l.setter, l.getter)
  }
}

object Lenser {
  def apply[A] = new Lenser[A]
}
