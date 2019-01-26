package org.jstott.mix.instructions.transfer

import org.jstott.mix._

case class ENNA(override val value: Word) extends ENN {

  val name = "ENNA"

  def compute(implicit mix: Mix): Mix = mix.copy(accumulator = computeValue(mix)).incrementProgramCounter

}

object ENNA {

  val operationCode = MixByte(48)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): ENNA = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENNA =
    ENNA(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}