package com.github.hexx.argonaut

import scalaz._, Scalaz._
import _root_.argonaut._
import _root_.argonaut.Argonaut._
import com.github.hexx.macros.{Name, Namer}

trait Encoder[A] extends (A => Json) {
  def encode[B: EncodeJson](f: A => B) = (implicitly[EncodeJson[B]] apply _)

  def name[B](f: Namer[A] => Name[B]) = f(new Namer[A]).name

  def value[B: EncodeJson](f: A => B) = (a: A) => encode(f) apply f(a)

  def assoc[B: EncodeJson](f1: Namer[A] => Name[B], f2: A => B) = (a: A) => (name(f1), value(f2) apply a)

  def getter[B](f: A => B) = f

  def apply(o: A) = jObjectAssocList(encoder.map(_ >>> (List(_))).reduce(_ |+| _).apply(o))

  def encoder: List[A => (JsonField, Json)]
}

trait Decoder[A] extends (HCursor => DecodeResult[A]) {
  def decode[B: DecodeJson](f: A => B) = (implicitly[DecodeJson[B]] apply _)

  def down[B](f: Namer[A] => Name[B]) = (c: HCursor) => c.downField(f(new Namer[A]).name)

  def field[B: DecodeJson](f: Namer[A] => Name[B]) = (c: HCursor) => implicitly[DecodeJson[B]] apply (c.downField(f(new Namer[A]).name).hcursor)
}
