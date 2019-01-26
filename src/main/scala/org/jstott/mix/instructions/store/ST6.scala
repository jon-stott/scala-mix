package org.jstott.mix.instructions.store

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ST6(override val value: Word) extends ST {

  val name = "ST6"

  def compute(implicit mix: Mix): Mix = {
    mix.withMemoryUpdated(computeStore(mix, Word(mix.index6)), addressWithIndex(mix)).incrementProgramCounter
  }

}

object ST6 {

  val operationCode = MixByte(30)

  def apply(address: TwoSignedBytes): ST6 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): ST6 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): ST6 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): ST6 =
    ST6(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}