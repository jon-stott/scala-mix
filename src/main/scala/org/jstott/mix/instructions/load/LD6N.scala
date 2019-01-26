package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD6N(override val value: Word) extends LD {

  val name = "LD6N"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index6 = TwoSignedBytes(computed.sign.invert, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD6N {

  val operationCode = MixByte(22)

  def apply(address: TwoSignedBytes): LD6N = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD6N = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD6N = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD6N =
    LD6N(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}