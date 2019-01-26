package org.jstott.mix.instructions.transfer

import org.jstott.mix._

case class ENNX(override val value: Word) extends ENN {

  val name = "ENNX"

  def compute(implicit mix: Mix): Mix = mix.copy(extension = computeValue(mix)).incrementProgramCounter

}

object ENNX {

  val operationCode = MixByte(55)

  val field = Field(0, 3)

  def apply(address: TwoSignedBytes): ENNX = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): ENNX =
    ENNX(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}