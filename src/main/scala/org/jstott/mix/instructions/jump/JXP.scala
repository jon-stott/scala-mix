package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JXP(override val value: Word) extends J {

  val name = "JXP"

  def compute(implicit mix: Mix): Mix = j_p(mix, mix.extension)

}

object JXP {

  val operationCode = MixByte(47)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): JXP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JXP =
    JXP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}