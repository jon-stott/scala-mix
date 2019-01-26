package org.jstott.mix

sealed trait Overflow

case object On extends Overflow

case object Off extends Overflow