package org.jstott.mix

case class TwoSignedBytes(sign: Sign = Plus, bytes: Seq[MixByte] = Seq.fill(2)(MixByte(0))) extends SignedBytes {

  require(bytes.length == 2, s"[${bytes.length}] bytes given when there should be exactly 2")

  def +(o: MixByte): TwoSignedBytes = TwoSignedBytes.apply(intValue + o.value)
  def +(o: TwoSignedBytes): TwoSignedBytes = TwoSignedBytes.apply(intValue + o.intValue)

  def intValue: Int = signAsMultiplier * (bytes(1).value + (bytes.head.value << 6))

  override def toString: String = {
    f"$sign ${bytes.head} ${bytes(1)} ($intValue%5d)"
  }

}

object TwoSignedBytes {

  val maxValue: Int = Math.pow(2, MixByte.bits * 2).toInt - 1
  val minValue: Int = -1 * maxValue

  def apply(i: Int): TwoSignedBytes = {
    val s = Sign(i)
    val pos = if (s == Minus) -1 * i else i
    val b0 = MixByte.fromInt(pos >> 6)
    val b1 = MixByte.fromInt(pos)
    val result = TwoSignedBytes(sign = s, bytes = Array(b0, b1))
    result
  }

  def apply(w: Word): TwoSignedBytes = {
    TwoSignedBytes(w.sign, w.bytes.takeRight(2))
  }

  def zero: TwoSignedBytes = TwoSignedBytes()

}