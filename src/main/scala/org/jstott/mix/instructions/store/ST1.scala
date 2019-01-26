package org.jstott.mix.instructions.store

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ST1(override val value: Word) extends ST {

  val name = "ST1"

  def compute(implicit mix: Mix): Mix = {
    mix.withMemoryUpdated(computeStore(mix, Word(mix.index1)), addressWithIndex(mix)).incrementProgramCounter
  }

}

object ST1 {

  val operationCode = MixByte(25)

  def apply(address: TwoSignedBytes): ST1 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): ST1 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): ST1 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): ST1 =
    ST1(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}