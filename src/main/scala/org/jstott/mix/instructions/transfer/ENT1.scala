package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ENT1(override val value: Word) extends ENT {

  val name = "ENT1"

  def compute(implicit mix: Mix): Mix = {
    val newVal = computeValue(mix)
    val newValSB = TwoSignedBytes(newVal)
    mix.copy(index1 = newValSB).incrementProgramCounter
  }

}

object ENT1 {

  val operationCode = MixByte(49)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): ENT1 = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENT1 =
    ENT1(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}