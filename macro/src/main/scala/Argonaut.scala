package com.github.hexx.argonaut.macros

import language.experimental.macros
import language.dynamics

import scala.reflect.macros.Context

import com.github.hexx.macros.TreeMaker

object Encoder {
  def all[T] = macro allImpl[T]
  def allImpl[T: c.WeakTypeTag](c: Context) = {
    c.Expr[Any](TreeMaker.mkEncodeAll(c)(implicitly[c.WeakTypeTag[T]].tpe))
  }
}
