package org.jstott.mix

sealed trait Sign {

  def invert: Sign

  def toString: String

}

case object Plus extends Sign {

  def invert: Sign = Minus

  override def toString: String = "+"

}

case object Minus extends Sign {

  def invert: Sign = Plus

  override def toString: String = "-"

}

object Sign {

  def apply(i: Int): Sign = if (i < 0) Minus else Plus

}