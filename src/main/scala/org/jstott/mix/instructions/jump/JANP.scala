package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JANP(override val value: Word) extends J {

  val name = "JANP"

  def compute(implicit mix: Mix): Mix = j_np(mix, mix.a)

}

object JANP {

  val operationCode = MixByte(40)

  val field = Field(0, 5)

  def apply(address: TwoSignedBytes): JANP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JANP =
    JANP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}