package org.jstott.mix.instructions.store

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ST3(override val value: Word) extends ST {

  val name = "ST3"

  def compute(implicit mix: Mix): Mix = {
    mix.withMemoryUpdated(computeStore(mix, Word(mix.index3)), addressWithIndex(mix)).incrementProgramCounter
  }

}

object ST3 {

  val operationCode = MixByte(27)

  def apply(address: TwoSignedBytes): ST3 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): ST3 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): ST3 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): ST3 =
    ST3(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}