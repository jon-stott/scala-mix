package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JAZ(override val value: Word) extends J {

  val name = "JAZ"

  def compute(implicit mix: Mix): Mix = j_z(mix, mix.a)

}

object JAZ {

  val operationCode = MixByte(40)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): JAZ = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JAZ =
    JAZ(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}