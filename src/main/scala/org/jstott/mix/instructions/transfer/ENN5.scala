package org.jstott.mix.instructions.transfer

import org.jstott.mix._

case class ENN5(override val value: Word) extends ENN {

  val name = "ENN5"

  def compute(implicit mix: Mix): Mix = mix.copy(index5 = TwoSignedBytes(computeValue(mix))).incrementProgramCounter

}

object ENN5 {

  val operationCode = MixByte(53)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): ENN5 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENN5 =
    ENN5(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}