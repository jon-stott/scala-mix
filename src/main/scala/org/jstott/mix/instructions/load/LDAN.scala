package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LDAN(override val value: Word) extends LD {

  val name = "LDAN"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(accumulator = computeLoad(mix, invertSign = true)).incrementProgramCounter
  }

}

object LDAN {

  val operationCode = MixByte(16)

  def apply(address: TwoSignedBytes): LDAN = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LDAN = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LDAN = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LDAN =
    LDAN(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}

