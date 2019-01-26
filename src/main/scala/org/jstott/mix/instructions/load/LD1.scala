package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD1(override val value: Word) extends LD {

  val name = "LD1"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index1 = TwoSignedBytes(computed.sign, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD1 {

  val operationCode = MixByte(9)

  def apply(address: TwoSignedBytes): LD1 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD1 = apply(address,index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD1 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD1 =
    LD1(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}