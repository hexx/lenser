package com.github.hexx.lenser

import scala.reflect.macros.Context

object TreeMaker {
  // (_1, _2)
  def mkTuple2(c: Context)(_1: c.Tree, _2: c.Tree) = {
    import c.universe._
    Apply(Select(Select(Ident(newTermName("scala")), newTermName("Tuple2")), newTermName("apply")), List(_1, _2))
  }

  // (name: tpe) => ...
  def mkParam(c: Context)(name: String, tpe: c.Type) = {
    import c.universe._
    ValDef(Modifiers(Flag.PARAM), newTermName(name), TypeTree(tpe), EmptyTree)
  }

  // (setter, getter)
  def mkLens(c: Context)(memberName: String, classType: c.Type, memberType: c.Type) = {
    import c.universe._

    // (a$: classType) => a$.memberName
    def mkGetter(memberName: String, classType: Type) =
      Function(List(mkParam(c)("a$", classType)), Select(Ident(newTermName("a$")), newTermName(memberName)))

    // (a$: classType, x$: memberType) => a$.copy(memberName = x$)
    def mkSetter(memberName: String, classType: Type, memberType: Type) =
      Function(List(mkParam(c)("a$", classType), mkParam(c)("x$", memberType)),
        Apply(Select(Ident(newTermName("a$")), newTermName("copy")),
          List(AssignOrNamedArg(Ident(newTermName(memberName)), Ident(newTermName("x$"))))))

    // scalaz.Lens.lensu(_1, _2)
    def mkLensu(_1: Tree, _2: Tree) =
      Apply(Select(Select(Ident(newTermName("scalaz")), newTermName("Lens")), newTermName("lensu")), List(_1, _2))

    mkLensu(mkSetter(memberName, classType, memberType), mkGetter(memberName, classType))
  }

  // implicitly[EncodeJson[memberType]]
  def mkEncode(c: Context)(memberType: c.Type) = {
    import c.universe._
    TypeApply(Ident(newTermName("implicitly")), List(AppliedTypeTree(Ident(newTypeName("EncodeJson")), List(TypeTree(memberType)))))
  }

  // (a$: classType) => (memberName, implicitly[EncodeJson[memberType]].apply(a$.memberName))
  def mkAssoc(c: Context)(memberName: String, classType: c.Type, memberType: c.Type) = {
    import c.universe._
    Function(List(mkParam(c)("a$", classType)),
      mkTuple2(c)(
        Literal(Constant(memberName)),
        Apply(Select(mkEncode(c)(memberType), newTermName("apply")), List(Select(Ident(newTermName("a$")), newTermName(memberName))))))
  }

  def getFieldInfo[T: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = {
    import c.universe._

    val classType = implicitly[WeakTypeTag[T]].tpe
    val Literal(Constant(memberName: String)) = propName.tree.asInstanceOf[Tree]
    val getterMember = classType.member(newTermName(memberName)) orElse {
      c.abort(c.enclosingPosition, "value " + memberName + " is not a member of " + classType)
    }
    val memberType = getterMember.typeSignatureIn(classType) match {
      case NullaryMethodType(memberType) => memberType
      case _                             => c.abort(c.enclosingPosition, "member %s is not a field".format(memberName))
    }
    (memberName, classType, memberType)
  }
}
