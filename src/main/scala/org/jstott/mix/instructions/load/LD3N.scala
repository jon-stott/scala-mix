package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD3N(override val value: Word) extends LD {

  val name = "LD3N"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index3 = TwoSignedBytes(computed.sign.invert, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD3N {

  val operationCode = MixByte(19)

  def apply(address: TwoSignedBytes): LD3N = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD3N = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD3N = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD3N =
    LD3N(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}