package org.jstott.mix.instructions.comparison

import org.jstott.mix._

case class CMPX(override val value: Word) extends CMP {

  val name = "CMPX"

  def compute(implicit mix: Mix): Mix = {
    mix.copy(comparisonIndicator = computeNewComparisonIndicator(mix, mix.a.intValue)).incrementProgramCounter
  }

}

object CMPX {

  val operationCode = MixByte(63)

  def apply(address: TwoSignedBytes): CMPX = apply(address, MixByte(0), Field.normal.toByte)
  def apply(address: TwoSignedBytes, index: MixByte): CMPX = apply(address, index, Field.normal.toByte)
  def apply(address: TwoSignedBytes, field: Field): CMPX = apply(address, MixByte(0), field.toByte)
  def apply(address: TwoSignedBytes, index: MixByte, field: MixByte): CMPX =
    CMPX(value = Word(address.sign, address.bytes ++ Seq(index, field, operationCode)))

}