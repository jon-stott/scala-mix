package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD2(override val value: Word) extends LD {

  val name = "LD2"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index2 = TwoSignedBytes(computed.sign, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD2 {

  val operationCode = MixByte(10)

  def apply(address: TwoSignedBytes): LD2 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD2 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD2 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD2 =
    LD2(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}