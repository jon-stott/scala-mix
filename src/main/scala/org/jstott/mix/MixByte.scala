package org.jstott.mix

import scala.math.pow

case class MixByte(value: Int) {

  require(value >= 0 && value <= (pow(2, MixByte.bits) - 1), s"byte value [$value] needs to be in range 0..${pow(2, MixByte.bits) - 1}")

  def asCharacter: String = {
    if (this > 55) {
      "#"
    } else {
      MixByte.characters.charAt(value).toString
    }
  }

  def <(o: MixByte): Boolean = value < o.value
  def <(o: Int): Boolean= value < o
  def <=(o: MixByte): Boolean = value <= o.value
  def <=(o: Int): Boolean= value <= o
  def ==(o: MixByte): Boolean = value == o.value
  def ==(o: Int): Boolean = value == o
  def >=(o: MixByte): Boolean = value <= o.value
  def >=(o: Int): Boolean = value <= o
  def >(o: MixByte): Boolean = value > o.value
  def >(o: Int): Boolean = value > o

  def +(o: MixByte): MixByte = MixByte.fromInt(value + o.value)
  def +(o: Int): MixByte = MixByte.fromInt(value + o)

  def <<(o: Int): MixByte = MixByte.fromInt(value << o)

  def asField: Field = Field(value / 8, value % 8)

  override def toString: String = f"$value%02d"

}

object MixByte {

  private val characters = " ABCDEFGHIΔJKLMNOPQRΣΠSTUVWXYZ0123456789.,()+-*/=$<>@;:'"

  val bits: Int = 6

  val maxValue: Int = pow(2, bits).toInt - 1

  def fromCharacter(c: Char): MixByte = {
    characters.indexOf(c) match {
      case -1 => MixByte(0)
      case x  => MixByte(x)
    }
  }

  def fromInt(i: Int): MixByte = MixByte(i & maxValue)

  def fromField(f: Field): MixByte = MixByte((f.left * 8) + f.right)

}

trait Bytes

trait UnsignedBytes extends Bytes {

  def bytes: Seq[MixByte]

  def intValue: Int

  def longValue: Long = intValue.toLong

}


trait SignedBytes extends UnsignedBytes {

  def sign: Sign

  protected def signAsMultiplier: Int = sign match {
    case Plus => 1
    case Minus => -1
  }

}