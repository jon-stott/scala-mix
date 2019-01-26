package org.jstott.mix.instructions.io

import org.jstott.mix.instructions.Instruction
import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class IOC(override val value: Word) extends Instruction {

  val name = "IOC"

  def compute(implicit mix: Mix): Mix = {
    mix.ioDevices.getIoDevice(mix, F.value).ioc(mix)
    mix.incrementProgramCounter
  }

}

object IOC {

  val operationCode = MixByte(35)

  def apply(address: TwoSignedBytes): IOC = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): IOC = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): IOC = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): IOC =
    IOC(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}