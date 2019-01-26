package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JMP(override val value: Word) extends J {

  val name = "JMP"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(
      programCounter = A.intValue,
      jump = TwoUnsignedBytes.fromInt(mix.programCounter + 1)
    )
  }

}

object JMP {

  val operationCode = MixByte(39)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): JMP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JMP =
    JMP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}