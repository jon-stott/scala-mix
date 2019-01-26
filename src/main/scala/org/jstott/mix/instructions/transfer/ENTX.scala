package org.jstott.mix.instructions.transfer

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class ENTX(override val value: Word) extends ENT {

  val name = "ENTX"

  def compute(implicit mix: Mix): Mix = mix.copy(extension = computeValue(mix)).incrementProgramCounter

}

object ENTX {

  val operationCode = MixByte(55)

  val field = Field(0, 2)

  def apply(address: TwoSignedBytes): ENTX = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENTX =
    ENTX(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}