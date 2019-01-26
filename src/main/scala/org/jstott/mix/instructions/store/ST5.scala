package org.jstott.mix.instructions.store

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ST5(override val value: Word) extends ST {

  val name = "ST5"

  def compute(implicit mix: Mix): Mix = {
    mix.withMemoryUpdated(computeStore(mix, Word(mix.index5)), addressWithIndex(mix)).incrementProgramCounter
  }

}

object ST5 {

  val operationCode = MixByte(29)

  def apply(address: TwoSignedBytes): ST5 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): ST5 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): ST5 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): ST5 =
    ST5(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}