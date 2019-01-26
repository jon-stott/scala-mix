package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD6(override val value: Word) extends LD {

  val name = "LD6"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index6 = TwoSignedBytes(computed.sign, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD6 {

  val operationCode = MixByte(14)

  def apply(address: TwoSignedBytes): LD6 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD6 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD6 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD6 =
    LD6(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}