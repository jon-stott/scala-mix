package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JAP(override val value: Word) extends J {

  val name = "JAP"

  def compute(implicit mix: Mix): Mix = j_p(mix, mix.a)

}

object JAP {

  val operationCode = MixByte(40)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): JAP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JAP =
    JAP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}