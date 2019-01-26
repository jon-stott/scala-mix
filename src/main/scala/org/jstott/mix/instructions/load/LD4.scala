package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD4(override val value: Word) extends LD {

  val name = "LD4"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index4 = TwoSignedBytes(computed.sign, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD4 {

  val operationCode = MixByte(12)

  def apply(address: TwoSignedBytes): LD4 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD4 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD4 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD4 =
    LD4(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}