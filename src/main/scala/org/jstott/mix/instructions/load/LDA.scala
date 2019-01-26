package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LDA(override val value: Word) extends LD {

  val name = "LDA"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(accumulator = computeLoad(mix)).incrementProgramCounter
  }

}

object LDA {

  val operationCode = MixByte(8)

  def apply(address: TwoSignedBytes): LDA = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LDA = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LDA = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LDA =
    LDA(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}