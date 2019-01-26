package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JOV(override val value: Word) extends J {

  val name = "JOV"

  def compute(implicit mix: Mix): Mix = {
    if (mix.overflow == On) {
      mix.copy(
        programCounter = mix.programCounter + A.intValue,
        jump = TwoUnsignedBytes.fromInt(mix.programCounter + 1),
        overflow = Off
      )
    } else {
      mix.incrementProgramCounter
    }
  }

}

object JOV {

  val operationCode = MixByte(39)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): JOV = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JOV =
    JOV(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}