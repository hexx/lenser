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

  def fieldName[T] = new FieldName[T]

  class FieldName[T] extends Dynamic {
    def selectDynamic(propName: String)  = macro FieldName.selectDynamic[T]
    def applyDynamic(propName: String)() = macro FieldName.applyDynamic[T]
  }

  object FieldName {
    def selectDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[T](c)(propName)()
    def applyDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String])() = {
      import c.universe._
      val (memberName, classType, _) = TreeMaker.getFieldInfo(c)(propName)
      c.Expr[Any](TreeMaker.mkName(c)(memberName, classType))
    }
  }

  def fieldValue[T] = new FieldValue[T]

  class FieldValue[T] extends Dynamic {
    def selectDynamic(propName: String)  = macro FieldValue.selectDynamic[T]
    def applyDynamic(propName: String)() = macro FieldValue.applyDynamic[T]
  }

  object FieldValue {
    def selectDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[T](c)(propName)()
    def applyDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String])() = {
      import c.universe._
      val (memberName, classType, memberType) = TreeMaker.getFieldInfo(c)(propName)
      c.Expr[Any](TreeMaker.mkValue(c)(memberName, classType, memberType))
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
