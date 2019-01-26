package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD5(override val value: Word) extends LD {

  val name = "LD5"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index5 = TwoSignedBytes(computed.sign, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD5 {

  val operationCode = MixByte(13)

  def apply(address: TwoSignedBytes): LD5 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD5 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD5 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD5 =
    LD5(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}