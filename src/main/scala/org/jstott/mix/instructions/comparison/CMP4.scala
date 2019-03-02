package org.jstott.mix.instructions.comparison

import org.jstott.mix._

case class CMP4(override val value: Word) extends CMP {

  val name = "CMP4"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(comparisonIndicator = computeNewComparisonIndicator(mix, mix.i4.intValue)).incrementProgramCounter
  }

}

object CMP4 {

  val operationCode = MixByte(60)

  def apply(address: TwoSignedBytes): CMP4 = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): CMP4 = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): CMP4 = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): CMP4 =
    CMP4(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}