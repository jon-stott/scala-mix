package org.jstott.mix.instructions.store

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class STA(override val value: Word) extends ST {

  val name = "STA"

  def compute(implicit mix: Mix): Mix = {
    mix.withMemoryUpdated(computeStore(mix, mix.accumulator), addressWithIndex(mix)).incrementProgramCounter
  }

}

object STA {

  val operationCode = MixByte(24)

  def apply(address: TwoSignedBytes): STA = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): STA = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): STA = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): STA =
    STA(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}