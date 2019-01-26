package org.jstott.mix.instructions.misc

import org.jstott.mix.instructions.Instruction
import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class NOP(override val value: Word) extends Instruction {

  val name = "NOP"

  def compute(implicit mix: Mix): Mix = {
    mix.incrementProgramCounter
  }

}

object NOP {

  val operationCode = MixByte(0)

  val field = Field(0, 0)

  def apply(): NOP = apply(TwoSignedBytes(0))
  def apply(address: TwoSignedBytes): NOP = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): NOP =
    NOP(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}