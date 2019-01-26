package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JANN(override val value: Word) extends J {

  val name = "JANN"

  def compute(implicit mix: Mix): Mix = j_nn(mix, mix.a)

}

object JANN {

  val operationCode = MixByte(40)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): JANN = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JANN =
    JANN(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}