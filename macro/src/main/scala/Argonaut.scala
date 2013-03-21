package com.github.hexx.macros

import language.experimental.macros
import language.dynamics

import scala.reflect.macros.Context

object Assocer {
  def all[T] = macro allImpl[T]
  def allImpl[T: c.WeakTypeTag](c: Context) = {
    c.Expr[Any](TreeMaker.mkAssocAll(c)(implicitly[c.WeakTypeTag[T]].tpe))
  }
}
