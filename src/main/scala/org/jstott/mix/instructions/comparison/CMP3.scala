package org.jstott.mix.instructions.comparison

import org.jstott.mix._

case class CMP3(override val value: Word) extends CMP {

  val name = "CMP3"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(comparisonIndicator = computeNewComparisonIndicator(mix, mix.a.intValue)).incrementProgramCounter
  }

}

object CMP3 {

  val operationCode = MixByte(59)

  def apply(address: TwoSignedBytes): CMP3 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): CMP3 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): CMP3 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): CMP3 =
    CMP3(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}