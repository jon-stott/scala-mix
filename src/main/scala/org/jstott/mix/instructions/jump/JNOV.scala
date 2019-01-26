package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JNOV(override val value: Word) extends J {

  val name = "JNOV"

  def compute(implicit mix: Mix): Mix = {
    if (mix.overflow == Off) {
      mix.copy(
        programCounter = mix.programCounter + A.intValue,
        jump = TwoUnsignedBytes.fromInt(mix.programCounter + 1)
      )
    } else {
      mix.copy(
        programCounter = mix.programCounter + 1,
        overflow = Off
      )
    }
  }

}

object JNOV {

  val operationCode = MixByte(39)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): JNOV = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JNOV =
    JNOV(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}