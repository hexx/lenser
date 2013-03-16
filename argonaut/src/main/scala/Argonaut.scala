package com.github.hexx.argonaut

import scalaz._, Scalaz._
import _root_.argonaut._
import _root_.argonaut.Argonaut._
import com.github.hexx.macros.argonaut._
import com.github.hexx.macros.argonaut.{Encoder => MacroEncoder}

trait Encoder[A] extends (A => Json) {
  def encode[B](f: MacroEncoder[A] => EncodeJson[B]) = {
    def toFunc1[B](e: EncodeJson[B]) = e apply _
    toFunc1(f(new MacroEncoder[A]))
  }

  def name(f: Namer[A] => String) = f(new Namer[A])

  def assoc(f: Assocer[A] => A => (JsonField, Json)) = f(new Assocer[A])

  def encoder: List[A => (JsonField, Json)]

  def getter[B](f: A => B) = f

  def apply(o: A) = jObjectAssocList(encoder.map(_ >>> (List(_))).reduce(_ |+| _).apply(o))
}
