package org.jstott.mix.instructions.transfer

import org.jstott.mix._

case class ENN4(override val value: Word) extends ENN {

  val name = "ENN4"

  def compute(implicit mix: Mix): Mix = mix.copy(index4 = TwoSignedBytes(computeValue(mix))).incrementProgramCounter

}

object ENN4 {

  val operationCode = MixByte(52)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): ENN4 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENN4 =
    ENN4(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}