package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD5N(override val value: Word) extends LD {

  val name = "LD5N"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index5 = TwoSignedBytes(computed.sign.invert, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD5N {

  val operationCode = MixByte(21)

  def apply(address: TwoSignedBytes): LD5N = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD5N = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD5N = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD5N =
    LD5N(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}