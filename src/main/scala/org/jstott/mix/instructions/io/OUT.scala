package org.jstott.mix.instructions.io

import org.jstott.mix.instructions.Instruction
import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class OUT(override val value: Word) extends Instruction {

  val name = "OUT"

  def compute(implicit mix: Mix): Mix = {
    mix.ioDevices.getIoDevice(mix, F.value).out(mix, addressWithIndex(mix))
    mix.incrementProgramCounter
  }

}

object OUT {

  val operationCode = MixByte(37)

  def apply(address: TwoSignedBytes): OUT = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): OUT = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): OUT = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): OUT =
    OUT(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}