package com.github.hexx.macros

import scala.reflect.macros.Context

object TreeMaker {
  // (setter, getter)
  def mkLens(c: Context)(memberName: String, classType: c.Type, memberType: c.Type) = {
    import c.universe._

    // (_1, _2)
    def mkTuple2(_1: Tree, _2: Tree) =
      Apply(Select(Select(Ident(newTermName("scala")), newTermName("Tuple2")), newTermName("apply")), List(_1, _2))

    // (name: tpe) => ...
    def mkParam(name: String, tpe: c.Type) =
      ValDef(Modifiers(Flag.PARAM), newTermName(name), TypeTree(tpe), EmptyTree)

    // (a$: classType, x$: memberType) => a$.copy(memberName = x$)
    def mkSetter(memberName: String, classType: Type, memberType: Type) =
      Function(List(mkParam("a$", classType), mkParam("x$", memberType)),
        Apply(Select(Ident(newTermName("a$")), newTermName("copy")),
          List(AssignOrNamedArg(Ident(newTermName(memberName)), Ident(newTermName("x$"))))))

    // (a$: classType) => a$.memberName
    def mkGetter(memberName: String, classType: Type) =
      Function(List(mkParam("a$", classType)), Select(Ident(newTermName("a$")), newTermName(memberName)))

    mkTuple2(mkSetter(memberName, classType, memberType), mkGetter(memberName, classType))
  }

  // Name[memberName].apply("memberName")
  def mkName(c: Context)(memberName: String, memberType: c.Type) = {
    import c.universe._

    Apply(
      TypeApply(
        Select(Select(Select(Select(Select(Ident(newTermName("com")), newTermName("github")), newTermName("hexx")), newTermName("macros")), newTermName("Name")), newTermName("apply")),
        List(TypeTree(memberType))),
      List(Literal(Constant(memberName))))
  }

  // Name[memberType].apply(memberName) :: ... :: HNil
  def mkNameAll(c: Context)(classType: c.Type) = {
    import c.universe._

    def mkHNil = Select(Ident(newTermName("shapeless")), newTermName("HNil"))

    def mkHlistOps = Select(Select(Ident(newTermName("shapeless")), newTermName("HList")), newTermName("hlistOps"))

    def mkHList(l: List[Tree]): Tree = l match {
      case Nil      => mkHNil
      case x :: Nil => Apply(Select(mkHNil, newTermName("$colon$colon")), List(x))
      case x :: xs  => Apply(Select(Apply(mkHlistOps, List(mkHList(xs))), newTermName("$colon$colon")), List(x))
    }

    mkHList(
      classType.members.map(_.asTerm).filter(_.isGetter).toList.map { getter =>
        val memberName = getter.name.encoded
        val NullaryMethodType(memberType) = getter.typeSignatureIn(classType)
        mkName(c)(memberName, memberType)
      }
    )
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
