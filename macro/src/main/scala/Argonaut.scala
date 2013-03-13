package com.github.hexx.lenser

import language.experimental.macros
import language.dynamics

import scala.reflect.macros.Context

object Argonaut {
  def encode[T] = new Encode[T]

  class Encode[T] extends Dynamic {
    def selectDynamic(propName: String)  = macro Encode.selectDynamic[T]
    def applyDynamic(propName: String)() = macro Encode.applyDynamic[T]
  }

  object Encode {
    def selectDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[T](c)(propName)()

    def applyDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String])() = {
      import c.universe._

      val (_, _, memberType) = TreeMaker.getFieldInfo(c)(propName)

      c.Expr[Any](TreeMaker.mkEncode(c)(memberType))
    }
  }

  def assoc[T] = new Assoc[T]

  class Assoc[T] extends Dynamic {
    def selectDynamic(propName: String)  = macro Assoc.selectDynamic[T]
    def applyDynamic(propName: String)() = macro Assoc.applyDynamic[T]
  }

  object Assoc {
    def selectDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[T](c)(propName)()

    def applyDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String])() = {
      import c.universe._

      val (memberName, classType, memberType) = TreeMaker.getFieldInfo(c)(propName)

      c.Expr[Any](TreeMaker.mkAssoc(c)(memberName, classType, memberType))
    }
  }
}
