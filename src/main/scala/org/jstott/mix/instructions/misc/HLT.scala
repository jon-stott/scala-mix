package org.jstott.mix.instructions.misc

import org.jstott.mix.instructions.Instruction
import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class HLT(override val value: Word) extends Instruction {

  val name = "HLT"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(terminate = true)
  }

}

object HLT {

  val operationCode = MixByte(5)

  val field = Field(0, 2)

  def apply(): HLT = apply(TwoSignedBytes(0))
  def apply(address: TwoSignedBytes): HLT = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): HLT =
    HLT(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}