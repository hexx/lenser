package com.github.hexx.macros

import scala.reflect.macros.Context

object TreeMaker {
  // (name: tpe) => ...
  def mkParam(c: Context)(name: String, tpe: c.Type) = {
    import c.universe._
    ValDef(Modifiers(Flag.PARAM), newTermName(name), TypeTree(tpe), EmptyTree)
  }

  // Lens(setter, getter, name)
  def mkLens(c: Context)(memberName: String, classType: c.Type, memberType: c.Type) = {
    import c.universe._

    // Lens.apply(setter, getter, name)
    def mkApplyLens(setter: Tree, getter: Tree, name: Tree) =
      Apply(Select(Select(Select(Select(Select(Ident(newTermName("com")), newTermName("github")), newTermName("hexx")), newTermName("macros")), newTermName("Lens")), newTermName("apply")), List(setter, getter, name))

    // (a$: classType, x$: memberType) => a$.copy(memberName = x$)
    def mkSetter(memberName: String, classType: Type, memberType: Type) =
      Function(List(mkParam(c)("a$", classType), mkParam(c)("x$", memberType)),
        Apply(Select(Ident(newTermName("a$")), newTermName("copy")),
          List(AssignOrNamedArg(Ident(newTermName(memberName)), Ident(newTermName("x$"))))))

    // (a$: classType) => a$.memberName
    def mkGetter(memberName: String, classType: Type) =
      Function(List(mkParam(c)("a$", classType)), Select(Ident(newTermName("a$")), newTermName(memberName)))

    mkApplyLens(mkSetter(memberName, classType, memberType), mkGetter(memberName, classType), Literal(Constant(memberName)))
  }

  // EncodeJson((a$: classType) => jObjectAssocList(List((memberName, implicitly[EncodeJson[memberType]].apply(a$.memberName)), ...)))
  def mkEncodeAll(c: Context)(classType: c.Type) = {
    import c.universe._

    // (_1, _2)
    def mkTuple2(_1: c.Tree, _2: c.Tree) =
      Apply(Select(Select(Ident(newTermName("scala")), newTermName("Tuple2")), newTermName("apply")), List(_1, _2))

    // implicitly[EncodeJson[memberType]]
    def mkEncode(memberType: c.Type) =
      TypeApply(Ident(newTermName("implicitly")), List(AppliedTypeTree(Ident(newTypeName("EncodeJson")), List(TypeTree(memberType)))))

    // (memberName, implicitly[EncodeJson[memberType]].apply(a$.memberName))
    def mkAssoc(memberName: String, memberType: c.Type) =
      mkTuple2(
        Literal(Constant(memberName)),
        Apply(Select(mkEncode(memberType), newTermName("apply")), List(Select(Ident(newTermName("a$")), newTermName(memberName))))
      )

    // List(...)
    def mkList(l: List[Tree]) =
      Apply(Select(Select(Select(Select(Ident(newTermName("scala")), newTermName("collection")), newTermName("immutable")), newTermName("List")), newTermName("apply")), l)

    Apply(Select(Ident(newTermName("argonaut")), newTermName("EncodeJson")),
      List(Function(List(mkParam(c)("a$", classType)),
        Apply(Select(Select(Ident(newTermName("argonaut")), newTermName("Argonaut")), newTermName("jObjectAssocList")),
          List(mkList(
            classType.members.map(_.asTerm).filter(_.isGetter).toList.map { getter =>
              val memberName = getter.name.encoded
              val NullaryMethodType(memberType) = getter.typeSignatureIn(classType)
              mkAssoc(memberName, memberType)
            }
          ))
        )
      ))
    )
  }

  def getFieldInfo[A: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = {
    import c.universe._

    val classType = implicitly[WeakTypeTag[A]].tpe
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
