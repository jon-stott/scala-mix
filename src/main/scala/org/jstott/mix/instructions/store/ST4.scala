package org.jstott.mix.instructions.store

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ST4(override val value: Word) extends ST {

  val name = "ST4"

  def compute(implicit mix: Mix): Mix = {
    mix.withMemoryUpdated(computeStore(mix, Word(mix.index4)), addressWithIndex(mix)).incrementProgramCounter
  }

}

object ST4 {

  val operationCode = MixByte(28)

  def apply(address: TwoSignedBytes): ST4 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): ST4 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): ST4 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): ST4 =
    ST4(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}