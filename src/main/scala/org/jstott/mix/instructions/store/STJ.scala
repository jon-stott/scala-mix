package org.jstott.mix.instructions.store

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class STJ(override val value: Word) extends ST {

  val name = "STJ"

  def compute(implicit mix: Mix): Mix = {
    val newMem = computeStore(mix, Word(mix.jump))
    mix.withMemoryUpdated(newMem, addressWithIndex(mix)).incrementProgramCounter
  }

}

object STJ {

  val operationCode = MixByte(32)

  def apply(address: TwoSignedBytes): STJ = apply(address, MixByte(0), Field(0, 2).toByte)
  def apply(address: TwoSignedBytes, index: MixByte): STJ = apply(address, index, Field(0, 2).toByte)
  def apply(address: TwoSignedBytes, field: Field): STJ = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): STJ =
    STJ(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}