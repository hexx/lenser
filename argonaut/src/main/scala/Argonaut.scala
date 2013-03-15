package com.github.hexx.argonaut

import scalaz._, Scalaz._
import _root_.argonaut._
import _root_.argonaut.Argonaut._
import com.github.hexx.lenser.Argonaut._

object Renderer {
  type ToName[T] = FieldName[T] => T => String
  type ToValue[T] = FieldValue[T] => T => Json
  type ToAssoc[T] = Assoc[T] => T => (String, Json)

  def toName[T](f: ToName[T]) = f(fieldName[T])

  def toValue[T](f: ToValue[T]) = f(fieldValue[T])

  def toAssoc[T](f: ToAssoc[T]) = f(assoc[T])

  def renderer[T](as: ToAssoc[T]*) = as.map(_(assoc[T])).map(_ >>> (List(_))).reduce(_ |+| _) >>> (jObjectAssocList(_))
}
