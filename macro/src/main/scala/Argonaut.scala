package com.github.hexx.macros.argonaut

import language.experimental.macros
import language.dynamics

import scala.reflect.macros.Context

import com.github.hexx.macros.TreeMaker

class Encoder[T] extends Dynamic {
  def selectDynamic(propName: String)  = macro Encoder.selectDynamic[T]
  def applyDynamic(propName: String)() = macro Encoder.applyDynamic[T]
}

object Encoder {
  def selectDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[T](c)(propName)()
  def applyDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String])() = {
    import c.universe._
    val (_, _, memberType) = TreeMaker.getFieldInfo(c)(propName)
    c.Expr[Any](TreeMaker.mkEncode(c)(memberType))
  }
}

class Namer[T] extends Dynamic {
  def selectDynamic(propName: String)  = macro Namer.selectDynamic[T]
  def applyDynamic(propName: String)() = macro Namer.applyDynamic[T]
}

object Namer {
  def selectDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[T](c)(propName)()
  def applyDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String])() = {
    import c.universe._
    val (memberName, classType, _) = TreeMaker.getFieldInfo(c)(propName)
    c.Expr[Any](TreeMaker.mkName(c)(memberName, classType))
  }
}

class Valuer[T] extends Dynamic {
  def selectDynamic(propName: String)  = macro Valuer.selectDynamic[T]
  def applyDynamic(propName: String)() = macro Valuer.applyDynamic[T]
}

object Valuer {
  def selectDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[T](c)(propName)()
  def applyDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String])() = {
    import c.universe._
    val (memberName, classType, memberType) = TreeMaker.getFieldInfo(c)(propName)
    c.Expr[Any](TreeMaker.mkValue(c)(memberName, classType, memberType))
  }
}

class Assocer[T] extends Dynamic {
  def selectDynamic(propName: String)  = macro Assocer.selectDynamic[T]
  def applyDynamic(propName: String)() = macro Assocer.applyDynamic[T]
}

object Assocer {
  def selectDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = applyDynamic[T](c)(propName)()
  def applyDynamic[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String])() = {
    import c.universe._
    val (memberName, classType, memberType) = TreeMaker.getFieldInfo(c)(propName)
    c.Expr[Any](TreeMaker.mkAssoc(c)(memberName, classType, memberType))
  }

  def all[T] = macro allImpl[T]
  def allImpl[T: c.WeakTypeTag](c: Context) = {
    c.Expr[Any](TreeMaker.mkAssocAll(c)(implicitly[c.WeakTypeTag[T]].tpe))
  }
}
