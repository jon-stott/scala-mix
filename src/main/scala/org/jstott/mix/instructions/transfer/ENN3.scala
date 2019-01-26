package org.jstott.mix.instructions.transfer

import org.jstott.mix._

case class ENN3(override val value: Word) extends ENN {

  val name = "ENN3"

  def compute(implicit mix: Mix): Mix = mix.copy(index3 = TwoSignedBytes(computeValue(mix))).incrementProgramCounter

}

object ENN3 {

  val operationCode = MixByte(51)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): ENN3 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENN3 =
    ENN3(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}