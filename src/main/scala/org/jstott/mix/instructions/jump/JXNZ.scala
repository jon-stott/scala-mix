package org.jstott.mix.instructions.jump

import org.jstott.mix._

case class JXNZ(override val value: Word) extends J {

  val name = "JXNZ"

  def compute(implicit mix: Mix): Mix = j_nz(mix, mix.extension)

}

object JXNZ {

  val operationCode = MixByte(47)

  val field = Field(0, 4)

  def apply(address: TwoSignedBytes): JXNZ = apply(address, MixByte(0))
  def apply(address: TwoSignedBytes, index: MixByte): JXNZ =
    JXNZ(value = Word(address.sign, address.bytes ++ Seq(index, field.toByte, operationCode)))

}