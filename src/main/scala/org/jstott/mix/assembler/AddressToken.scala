package org.jstott.mix.assembler

sealed trait AddressToken

case class SymbolToken(s: String) extends AddressToken
case class NumberToken(n: Int) extends AddressToken
case object PlusToken extends AddressToken
case object MinusToken extends  AddressToken
case object AsteriskToken extends AddressToken
case object DivideToken extends AddressToken
case object FractionToken extends AddressToken
case object ColonToken extends AddressToken
case object CommaToken extends AddressToken
case object OpenParenthesisToken extends AddressToken
case object CloseParenthesisToken extends AddressToken
case object EqualsToken extends AddressToken