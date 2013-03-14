package com.github.hexx.argonaut

import scalaz._, Scalaz._
import _root_.argonaut._
import _root_.argonaut.Argonaut._
import com.github.hexx.lenser.Argonaut._

object Renderer {
  type ToAssoc[T] = Assoc[T] => T => JsonAssoc

  def renderer[T](as: ToAssoc[T]*) = as.map(_(assoc[T])).map(_ >>> (List(_))).reduce(_ |+| _) >>> (jObjectAssocList(_))
}
