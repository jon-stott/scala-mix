package org.jstott.mix.instructions.store

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ST2(override val value: Word) extends ST {

  val name = "ST2"

  def compute(implicit mix: Mix): Mix = {
    val toStore = computeStore(mix, Word(mix.index2))
    mix.withMemoryUpdated(toStore, addressWithIndex(mix)).incrementProgramCounter
  }

}

object ST2 {

  val operationCode = MixByte(26)

  def apply(address: TwoSignedBytes): ST2 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): ST2 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): ST2 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): ST2 =
    ST2(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}