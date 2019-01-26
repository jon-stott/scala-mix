package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JSJ(override val value: Word) extends J {

  val name = "JSJ"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(
      programCounter = mix.programCounter + A.intValue
    )
  }

}

object JSJ {

  val operationCode = MixByte(39)

  val field = Field(0, 1)

  def apply(address: TwoSignedBytes): JSJ = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JSJ =
    JSJ(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}