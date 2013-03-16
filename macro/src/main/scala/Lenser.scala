package com.github.hexx.macros.lenser

import language.experimental.macros
import language.dynamics

import scala.reflect.macros.Context

import com.github.hexx.macros.TreeMaker

class Lenser[T] extends Dynamic {
  def selectDynamic(propName: String)  = macro Lenser.selectDynamic[T]
  def applyDynamic(propName: String)() = macro Lenser.applyDynamic[T]
}

object Lenser {
  def lenser[T] = new Lenser[T]

  def selectDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[T](c)(propName)()

  def applyDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String])() = {
    import c.universe._

    val (memberName, classType, memberType) = TreeMaker.getFieldInfo(c)(propName)

    c.Expr[Any](c.resetAllAttrs(TreeMaker.mkLens(c)(memberName, classType, memberType)))
  }
}
