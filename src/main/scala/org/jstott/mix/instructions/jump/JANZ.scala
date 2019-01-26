package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JANZ(override val value: Word) extends J {

  val name = "JANZ"

  def compute(implicit mix: Mix): Mix = j_nz(mix, mix.a)

}

object JANZ {

  val operationCode = MixByte(40)

  val field = Field(0, 4)

  def apply(address: TwoSignedBytes): JANZ = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JANZ =
    JANZ(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}