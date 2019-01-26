package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LD3(override val value: Word) extends LD {

  val name = "LD3"

  def compute(implicit mix: Mix): Mix = {
    val computed = computeLoad(mix)
    mix.copy(index3 = TwoSignedBytes(computed.sign, computed.bytes.slice(3, 5))).incrementProgramCounter
  }

}

object LD3 {

  val operationCode = MixByte(11)

  def apply(address: TwoSignedBytes): LD3 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LD3 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LD3 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LD3 =
    LD3(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}