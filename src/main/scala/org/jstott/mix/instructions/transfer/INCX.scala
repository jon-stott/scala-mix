package org.jstott.mix.instructions.transfer

import org.jstott.mix.{Accumulator, MixByte, Extension, Field, Mix, TwoSignedBytes, Word}

case class INCX(override val value: Word) extends INC {

  val name = "INCX"

  def compute(implicit mix: Mix): Mix = computeWord(mix, Extension, mix.x).incrementProgramCounter

}

object INCX {

  val operationCode = MixByte(55)

  val field = Field(0, 0)

  def apply(address: TwoSignedBytes): INCX = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): INCX =
    INCX(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}