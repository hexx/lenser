package com.github.hexx.macros

import language.experimental.macros
import language.dynamics

import scala.reflect.macros.Context

case class Name[T](name: String)

class Namer[T] extends Dynamic {
  def selectDynamic(propName: String)  = macro Namer.selectDynamic[T]
  def applyDynamic(propName: String)() = macro Namer.applyDynamic[T]
}

object Namer {
  def name[T] = new Namer[T]

  def selectDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[T](c)(propName)()

  def applyDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String])() = {
    import c.universe._

    val (memberName, classType, memberType) = TreeMaker.getFieldInfo(c)(propName)

    c.Expr[Any](c.resetAllAttrs(TreeMaker.mkName(c)(memberName, memberType)))
  }

  def all[T] = macro allImpl[T]

  def allImpl[T: c.WeakTypeTag](c: Context) = c.Expr[Any](TreeMaker.mkNameAll(c)(implicitly[c.WeakTypeTag[T]].tpe))
}
