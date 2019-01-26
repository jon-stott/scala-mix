package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LDX(override val value: Word) extends LD {

  val name = "LDX"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(extension = computeLoad(mix)).incrementProgramCounter
  }

}

object LDX {

  val operationCode = MixByte(15)

  def apply(address: TwoSignedBytes): LDX = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LDX = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LDX = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LDX =
    LDX(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}

