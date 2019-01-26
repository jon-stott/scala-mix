package org.jstott.mix.instructions.transfer

import org.jstott.mix._

case class ENN1(override val value: Word) extends ENN {

  val name = "ENN1"

  def compute(implicit mix: Mix): Mix = {
    val newVal = computeValue(mix)
    val newValSB = TwoSignedBytes(newVal)
    mix.copy(index1 = newValSB).incrementProgramCounter
  }

}

object ENN1 {

  val operationCode = MixByte(49)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): ENN1 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENN1 =
    ENN1(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}