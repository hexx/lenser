package com.github.hexx.argonaut

import scalaz._, Scalaz._
import _root_.argonaut._
import _root_.argonaut.Argonaut._
import com.github.hexx.macros.{Lenser, Assocer, Lens => MacroLens}
import com.github.hexx.macros.Lenser._

trait Encoder[A] extends (A => Json) {
  def encode[B: EncodeJson](f: A => B) = (implicitly[EncodeJson[B]] apply _)

  def name[B](f: ToLenser[A, B]) = Lenser.lens(f).name

  def value[B: EncodeJson](f: A => B) = (a: A) => encode(f) apply f(a)

  def assoc[B: EncodeJson](f: ToLenser[A, B]) = {
    val l = Lenser.lens(f)
    (a: A) => (name(f), value(l.getter) apply a)
  }

  def getter[B](f: A => B) = f

  def apply(o: A) = jObjectAssocList(encoder.map(_ >>> (List(_))).reduce(_ |+| _).apply(o))

  def all = Assocer.all[A]

  def encoder: List[A => (JsonField, Json)]
}

trait Decoder[A] extends (HCursor => DecodeResult[A]) {
  def decode[B: DecodeJson](f: A => B) = (implicitly[DecodeJson[B]] apply _)

  def down[B](f: ToLenser[A, B]) = (c: HCursor) => c.downField(Lenser.lens(f).name)

  def field[B: DecodeJson](f: ToLenser[A, B]) = (c: HCursor) => implicitly[DecodeJson[B]] apply (down(f) apply c).hcursor
}
