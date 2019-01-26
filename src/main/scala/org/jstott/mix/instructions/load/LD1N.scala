package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD1N(override val value: Word) extends LD {

  val name = "LD1N"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index1 = TwoSignedBytes(computed.sign.invert, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD1N {

  val operationCode = MixByte(17)

  def apply(address: TwoSignedBytes): LD1N = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD1N = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD1N = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD1N =
    LD1N(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}