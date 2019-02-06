package org.jstott.mix.instructions.store

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class STZ(override val value: Word) extends ST {

  val name = "STZ"

  def compute(implicit mix: Mix): Mix = {
    mix.withMemoryUpdated(Word(), addressWithIndex(mix)).incrementProgramCounter
  }

}

object STZ {

  val operationCode = MixByte(33)

  def apply(address: TwoSignedBytes): STZ = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): STZ = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): STZ = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): STZ =
    STZ(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}
