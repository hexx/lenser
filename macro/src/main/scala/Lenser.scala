package com.github.hexx.macros

import language.experimental.macros
import language.dynamics

import scala.reflect.macros.Context

case class Lens[A, B](setter: (A, B) => A, getter: A => B, name: String)

class Lenser[A] extends Dynamic {
  def selectDynamic(propName: String)  = macro Lenser.selectDynamic[A]
  def applyDynamic(propName: String)() = macro Lenser.applyDynamic[A]
}

object Lenser {
  type ToLenser[A, B] = Lenser[A] => Lens[A, B]

  def lenser[A] = new Lenser[A]

  def lens[A, B](f: ToLenser[A, B]) = f(new Lenser[A])

  def selectDynamic[A: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[A](c)(propName)()

  def applyDynamic[A: c.WeakTypeTag](c: Context)(propName: c.Expr[String])() = {
    import c.universe._

    val (memberName, classType, memberType) = TreeMaker.getFieldInfo(c)(propName)

    c.Expr[Any](c.resetAllAttrs(TreeMaker.mkLens(c)(memberName, classType, memberType)))
  }
}
