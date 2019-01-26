package org.jstott.mix.instructions.conversion

import org.jstott.mix.instructions.Instruction
import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class NUM(override val value: Word) extends Instruction {

  val name = "NUM"

  def compute(implicit mix: Mix): Mix = {
    mix // TODO implement NUM compute
  }

}

object NUM {

  val operationCode = MixByte(5)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): NUM = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): NUM =
    NUM(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}