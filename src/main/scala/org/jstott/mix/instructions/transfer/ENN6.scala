package org.jstott.mix.instructions.transfer

import org.jstott.mix._

case class ENN6(override val value: Word) extends ENN {

  val name = "ENN6"

  def compute(implicit mix: Mix): Mix = mix.copy(index6 = TwoSignedBytes(computeValue(mix))).incrementProgramCounter

}

object ENN6 {

  val operationCode = MixByte(54)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): ENN6 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENN6 =
    ENN6(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}