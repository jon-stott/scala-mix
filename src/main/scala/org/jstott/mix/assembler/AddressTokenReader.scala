package org.jstott.mix.assembler

import scala.util.parsing.input.{NoPosition, Position, Reader}

class AddressTokenReader(tokens: Seq[AddressToken]) extends Reader[AddressToken] {

  override def first: AddressToken = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = NoPosition

  override def rest: Reader[AddressToken] = new AddressTokenReader(tokens.tail)

}