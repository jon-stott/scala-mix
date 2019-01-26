package org.jstott.mix.instructions.load

import org.jstott.mix.{MixByte, Field, Mix, TwoSignedBytes, Word}

case class LDXN(override val value: Word) extends LD {

  val name = "LDXN"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(extension = computeLoad(mix, invertSign = true)).incrementProgramCounter
  }

}

object LDXN {

  val operationCode = MixByte(23)

  def apply(address: TwoSignedBytes): LDXN = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): LDXN = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): LDXN = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): LDXN =
    LDXN(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}



