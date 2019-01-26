package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JXZ(override val value: Word) extends J {

  val name = "JXZ"

  def compute(implicit mix: Mix): Mix = j_z(mix, mix.extension)

}

object JXZ {

  val operationCode = MixByte(47)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): JXZ = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JXZ =
    JXZ(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}