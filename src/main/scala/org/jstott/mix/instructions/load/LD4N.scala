package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD4N(override val value: Word) extends LD {

  val name = "LD4N"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index4 = TwoSignedBytes(computed.sign.invert, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD4N {

  val operationCode = MixByte(20)

  def apply(address: TwoSignedBytes): LD4N = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD4N = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD4N = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD4N =
    LD4N(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}