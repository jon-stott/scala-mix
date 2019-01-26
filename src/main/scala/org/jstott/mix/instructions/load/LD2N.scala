package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD2N(override val value: Word) extends LD {

  val name = "LD2N"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index2 = TwoSignedBytes(computed.sign.invert, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD2N {

  val operationCode = MixByte(18)

  def apply(address: TwoSignedBytes): LD2N = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD2N = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD2N = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD2N =
    LD2N(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}