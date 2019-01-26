package org.jstott.mix

sealed trait Register

sealed trait RegisterWord extends Register
sealed trait RegisterTwoSignedBytes extends Register
sealed trait RegisterTwoUnsignedBytes extends Register

case object Accumulator extends RegisterWord
case object Extension extends RegisterWord
case object Index1 extends RegisterTwoSignedBytes
case object Index2 extends RegisterTwoSignedBytes
case object Index3 extends RegisterTwoSignedBytes
case object Index4 extends RegisterTwoSignedBytes
case object Index5 extends RegisterTwoSignedBytes
case object Index6 extends RegisterTwoSignedBytes
case object Jump extends RegisterTwoUnsignedBytes
