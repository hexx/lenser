package com.github.hexx.scalaz

import scalaz._, Scalaz._

import com.github.hexx.macros.{Lenser => MacroLenser}

class Lenser[A] {
  def lens[B](f: MacroLenser[A] => ((A, B) => A, A => B)) = {
    val (setter, getter) = f(MacroLenser.lens[A])
    Lens.lensu(setter, getter)
  }
}

object Lenser {
  def apply[A] = new Lenser[A]
}
