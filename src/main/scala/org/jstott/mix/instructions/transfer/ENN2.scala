package org.jstott.mix.instructions.transfer

import org.jstott.mix._

case class ENN2(override val value: Word) extends ENN {

  val name = "ENN2"

  def compute(implicit mix: Mix): Mix = mix.copy(index2 = TwoSignedBytes(computeValue(mix))).incrementProgramCounter

}

object ENN2 {

  val operationCode = MixByte(50)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): ENN2 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENN2 =
    ENN2(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}