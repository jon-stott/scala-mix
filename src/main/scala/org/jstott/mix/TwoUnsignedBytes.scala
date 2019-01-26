package org.jstott.mix

case class TwoUnsignedBytes(bytes: Seq[MixByte] = Seq.fill(2)(MixByte(0))) extends UnsignedBytes {

  require(bytes.length == 2, s"[${bytes.length}] bytes given when there should be exactly 2")

  def +(o: MixByte): TwoUnsignedBytes = TwoUnsignedBytes.fromInt(intValue + o.value)

  def intValue: Int = (bytes.head << 6).value + bytes(1).value

  override def toString: String = {
    f"  ${bytes.head} ${bytes(1)} ($intValue%5d)"
  }

}

object TwoUnsignedBytes {

  def fromInt(i: Int): TwoUnsignedBytes = {
    val b0 = MixByte.fromInt(i >> 6)
    val b1 = MixByte.fromInt(i)
    TwoUnsignedBytes(bytes = Array(b0, b1))
  }

}

