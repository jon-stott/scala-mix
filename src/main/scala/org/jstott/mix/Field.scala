package org.jstott.mix

case class Field(left: Int, right: Int) {

  // FIXME can left be greater than right?
//  require(left <= right, s"left [$left] must be <= right [$right]")
  require(left >= 0 && left <= 5, s"left [$left] must be >= 0 and <= 5")
  require(right >= 0 && right <= 7, s"right [$right] must be >= 0 and <= 7")

  def toByte: MixByte = MixByte.fromField(this)

  def intValue: Int = (left * 8) + right

  override def toString: String = {
    if (left == 0 && right == 5) {
      ""
    } else {
      s"($left:$right)"
    }
  }

}

object Field {

  val normal = Field(0, 5)

  def apply(f: Int): Field = Field(f / 8, f % 8)

}
