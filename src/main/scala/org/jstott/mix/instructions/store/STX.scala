package org.jstott.mix.instructions.store

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class STX(override val value: Word) extends ST {

  val name = "STX"

  def compute(implicit mix: Mix): Mix = {
    mix.withMemoryUpdated(computeStore(mix, mix.extension), addressWithIndex(mix)).incrementProgramCounter
  }

}

object STX {

  val operationCode = MixByte(31)

  def apply(address: TwoSignedBytes): STX = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): STX = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): STX = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): STX =
    STX(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}



