package org.jstott.mix.assembler

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object MixParser extends RegexParsers {

  override def skipWhitespace: Boolean = false

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  def w: Parser[WhitespaceToken.type] = whiteSpace ^^ { s => WhitespaceToken }
  def newLine: Parser[NewLineToken.type] = "\n".r ^^ { s => NewLineToken }

  def location: Parser[LocationToken] = "\\d*[A-Z][A-Z0-9]*".r ^^ { s => LocationToken(s) }

  def char: Parser[CharToken.type] = "CHAR" ^^ { _ => CharToken }
  def cmpa: Parser[CmpaToken.type] = "CMPA" ^^ { _ => CmpaToken }
  def con: Parser[ConToken.type] = "CON" ^^ { _ => ConToken }
  def dec1: Parser[Dec1Token.type] = "DEC1" ^^ { _ => Dec1Token }
  def dec2: Parser[Dec2Token.type] = "DEC2" ^^ { _ => Dec2Token }
  def dec3: Parser[Dec3Token.type] = "DEC3" ^^ { _ => Dec3Token }
  def dec4: Parser[Dec4Token.type] = "DEC4" ^^ { _ => Dec4Token }
  def dec5: Parser[Dec5Token.type] = "DEC5" ^^ { _ => Dec5Token }
  def dec6: Parser[Dec6Token.type] = "DEC6" ^^ { _ => Dec6Token }
  def deca: Parser[DecaToken.type] = "DECA" ^^ { _ => DecaToken }
  def decx: Parser[DecxToken.type] = "DECX" ^^ { _ => DecxToken }
  def div: Parser[DivToken.type] = "DIV" ^^ { _ => DivToken }
  def end: Parser[EndToken.type] = "END" ^^ { _ => EndToken }
  def enn1: Parser[Enn1Token.type] = "ENN1" ^^ { _ => Enn1Token }
  def enn2: Parser[Enn2Token.type] = "ENN2" ^^ { _ => Enn2Token }
  def enn3: Parser[Enn3Token.type] = "ENN3" ^^ { _ => Enn3Token }
  def enn4: Parser[Enn4Token.type] = "ENN4" ^^ { _ => Enn4Token }
  def enn5: Parser[Enn5Token.type] = "ENN5" ^^ { _ => Enn5Token }
  def enn6: Parser[Enn6Token.type] = "ENN6" ^^ { _ => Enn6Token }
  def enna: Parser[EnnaToken.type] = "ENNA" ^^ { _ => EnnaToken }
  def ennx: Parser[EnnxToken.type] = "ENNX" ^^ { _ => EnnxToken }
  def ent1: Parser[Ent1Token.type] = "ENT1" ^^ { _ => Ent1Token }
  def ent2: Parser[Ent2Token.type] = "ENT2" ^^ { _ => Ent2Token }
  def ent3: Parser[Ent3Token.type] = "ENT3" ^^ { _ => Ent3Token }
  def ent4: Parser[Ent4Token.type] = "ENT4" ^^ { _ => Ent4Token }
  def ent5: Parser[Ent5Token.type] = "ENT5" ^^ { _ => Ent5Token }
  def ent6: Parser[Ent6Token.type] = "ENT6" ^^ { _ => Ent6Token }
  def enta: Parser[EntaToken.type] = "ENTA" ^^ { _ => EntaToken }
  def entx: Parser[EntxToken.type] = "ENTX" ^^ { _ => EntxToken }
  def equ: Parser[EquToken.type] = "EQU" ^^ { _ => EquToken }
  def hlt: Parser[HltToken.type] = "HLT" ^^ { _ => HltToken }
  def inc1: Parser[Inc1Token.type] = "INC1" ^^ { _ => Inc1Token }
  def inc2: Parser[Inc2Token.type] = "INC2" ^^ { _ => Inc2Token }
  def inc3: Parser[Inc3Token.type] = "INC3" ^^ { _ => Inc3Token }
  def inc4: Parser[Inc4Token.type] = "INC4" ^^ { _ => Inc4Token }
  def inc5: Parser[Inc5Token.type] = "INC5" ^^ { _ => Inc5Token }
  def inc6: Parser[Inc6Token.type] = "INC6" ^^ { _ => Inc6Token }
  def inca: Parser[IncaToken.type] = "INCA" ^^ { _ => IncaToken }
  def incx: Parser[IncxToken.type] = "INCX" ^^ { _ => IncxToken }
  def ioc: Parser[IocToken.type] = "IOC" ^^ { _ => IocToken }
  def j1n: Parser[J1nToken.type] = "J1N" ^^ { _ => J1nToken }
  def j1nn: Parser[J1nnToken.type] = "J1NN" ^^ { _ => J1nnToken }
  def j1np: Parser[J1npToken.type] = "J1NP" ^^ { _ => J1npToken }
  def j1nz: Parser[J1nzToken.type] = "J1NZ" ^^ { _ => J1nzToken }
  def j1p: Parser[J1pToken.type] = "J1P" ^^ { _ => J1pToken }
  def j1z: Parser[J1zToken.type] = "J1Z" ^^ { _ => J1zToken }
  def j2n: Parser[J2nToken.type] = "J2N" ^^ { _ => J2nToken }
  def j2nn: Parser[J2nnToken.type] = "J2NN" ^^ { _ => J2nnToken }
  def j2np: Parser[J2npToken.type] = "J2NP" ^^ { _ => J2npToken }
  def j2nz: Parser[J2nzToken.type] = "J2NZ" ^^ { _ => J2nzToken }
  def j2p: Parser[J2pToken.type] = "J2P" ^^ { _ => J2pToken }
  def j2z: Parser[J2zToken.type] = "J2Z" ^^ { _ => J2zToken }
  def j3n: Parser[J3nToken.type] = "J3N" ^^ { _ => J3nToken }
  def j3nn: Parser[J3nnToken.type] = "J3NN" ^^ { _ => J3nnToken }
  def j3np: Parser[J3npToken.type] = "J3NP" ^^ { _ => J3npToken }
  def j3nz: Parser[J3nzToken.type] = "J3NZ" ^^ { _ => J3nzToken }
  def j3p: Parser[J3pToken.type] = "J3P" ^^ { _ => J3pToken }
  def j3z: Parser[J3zToken.type] = "J3Z" ^^ { _ => J3zToken }
  def j4n: Parser[J4nToken.type] = "J4N" ^^ { _ => J4nToken }
  def j4nn: Parser[J4nnToken.type] = "J4NN" ^^ { _ => J4nnToken }
  def j4np: Parser[J4npToken.type] = "J4NP" ^^ { _ => J4npToken }
  def j4nz: Parser[J4nzToken.type] = "J4NZ" ^^ { _ => J4nzToken }
  def j4p: Parser[J4pToken.type] = "J4P" ^^ { _ => J4pToken }
  def j4z: Parser[J4zToken.type] = "J4Z" ^^ { _ => J4zToken }
  def j5n: Parser[J5nToken.type] = "J5N" ^^ { _ => J5nToken }
  def j5nn: Parser[J5nnToken.type] = "J5NN" ^^ { _ => J5nnToken }
  def j5np: Parser[J5npToken.type] = "J5NP" ^^ { _ => J5npToken }
  def j5nz: Parser[J5nzToken.type] = "J5NZ" ^^ { _ => J5nzToken }
  def j5p: Parser[J5pToken.type] = "J5P" ^^ { _ => J5pToken }
  def j5z: Parser[J5zToken.type] = "J5Z" ^^ { _ => J5zToken }
  def j6n: Parser[J6nToken.type] = "J6N" ^^ { _ => J6nToken }
  def j6nn: Parser[J6nnToken.type] = "J6NN" ^^ { _ => J6nnToken }
  def j6np: Parser[J6npToken.type] = "J6NP" ^^ { _ => J6npToken }
  def j6nz: Parser[J6nzToken.type] = "J6NZ" ^^ { _ => J6nzToken }
  def j6p: Parser[J6pToken.type] = "J6P" ^^ { _ => J6pToken }
  def j6z: Parser[J6zToken.type] = "J6Z" ^^ { _ => J6zToken }
  def jan: Parser[JanToken.type] = "JAN" ^^ { _ => JanToken }
  def jann: Parser[JannToken.type] = "JANN" ^^ { _ => JannToken }
  def janp: Parser[JanpToken.type] = "JANP" ^^ { _ => JanpToken }
  def janz: Parser[JanzToken.type] = "JANZ" ^^ { _ => JanzToken }
  def jap: Parser[JapToken.type] = "JAP" ^^ { _ => JapToken }
  def jaz: Parser[JazToken.type] = "JAZ" ^^ { _ => JazToken }
  def je: Parser[JeToken.type] = "JE" ^^ { _ => JeToken }
  def jg: Parser[JgToken.type] = "JG" ^^ { _ => JgToken }
  def jge: Parser[JgeToken.type] = "JGE" ^^ { _ => JgeToken }
  def jl: Parser[JlToken.type] = "JL" ^^ { _ => JlToken }
  def jle: Parser[JleToken.type] = "JLE" ^^ { _ => JleToken }
  def jmp: Parser[JmpToken.type] = "JMP" ^^ { _ => JmpToken }
  def jne: Parser[JneToken.type] = "JNE" ^^ { _ => JneToken }
  def jxn: Parser[JxnToken.type] = "JXN" ^^ { _ => JxnToken }
  def jxnn: Parser[JxnnToken.type] = "JXNN" ^^ { _ => JxnnToken }
  def jxnp: Parser[JxnpToken.type] = "JXNP" ^^ { _ => JxnpToken }
  def jxnz: Parser[JxnzToken.type] = "JXNZ" ^^ { _ => JxnzToken }
  def jxp: Parser[JxpToken.type] = "JXP" ^^ { _ => JxpToken }
  def jxz: Parser[JxzToken.type] = "JXZ" ^^ { _ => JxzToken }
  def ld1: Parser[Ld1Token.type] = "LD1" ^^ { _ => Ld1Token }
  def ld1n: Parser[Ld1nToken.type] = "LD1N" ^^ { _ => Ld1nToken }
  def ld2: Parser[Ld2Token.type] = "LD2" ^^ { _ => Ld2Token }
  def ld2n: Parser[Ld2nToken.type] = "LD2N" ^^ { _ => Ld2nToken }
  def ld3: Parser[Ld3Token.type] = "LD3" ^^ { _ => Ld3Token }
  def ld3n: Parser[Ld3nToken.type] = "LD3N" ^^ { _ => Ld3nToken }
  def ld4: Parser[Ld4Token.type] = "LD4" ^^ { _ => Ld4Token }
  def ld4n: Parser[Ld4nToken.type] = "LD4N" ^^ { _ => Ld4nToken }
  def ld5: Parser[Ld5Token.type] = "LD5" ^^ { _ => Ld5Token }
  def ld5n: Parser[Ld5nToken.type] = "LD5N" ^^ { _ => Ld5nToken }
  def ld6: Parser[Ld6Token.type] = "LD6" ^^ { _ => Ld6Token }
  def ld6n: Parser[Ld6nToken.type] = "LD6N" ^^ { _ => Ld6nToken }
  def lda: Parser[LdaToken.type] = "LDA" ^^ { _ => LdaToken }
  def ldan: Parser[LdanToken.type] = "LDAN" ^^ { _ => LdanToken }
  def ldx: Parser[LdxToken.type] = "LDX" ^^ { _ => LdxToken }
  def ldxn: Parser[LdxnToken.type] = "LDXN" ^^ { _ => LdxnToken }
  def num: Parser[NumToken.type] = "NUM" ^^ { _ => NumToken }
  def orig: Parser[OrigToken.type] = "ORIG" ^^ { _ => OrigToken }
  def out: Parser[OutToken.type] = "OUT" ^^ { _ => OutToken }
  def st1: Parser[St1Token.type] = "ST1" ^^ { _ => St1Token }
  def st2: Parser[St2Token.type] = "ST2" ^^ { _ => St2Token }
  def st3: Parser[St3Token.type] = "ST3" ^^ { _ => St3Token }
  def st4: Parser[St4Token.type] = "ST4" ^^ { _ => St4Token }
  def st5: Parser[St5Token.type] = "ST5" ^^ { _ => St5Token }
  def st6: Parser[St6Token.type] = "ST6" ^^ { _ => St6Token }
  def sta: Parser[StaToken.type] = "STA" ^^ { _ => StaToken }
  def stj: Parser[StjToken.type] = "STJ" ^^ { _ => StjToken }
  def stx: Parser[StxToken.type] = "STX" ^^ { _ => StxToken }
  def stz: Parser[StzToken.type] = "STZ" ^^ { _ => StzToken }

  def operation: Parser[OperationToken] = char | cmpa | con | dec1 | dec2 | dec3 | dec4 | dec5 | dec6 | deca |
    decx | div | end | enn1 | enn2 | enn3 | enn4 | enn5 | enn6 | enna | ennx | ent1 | ent2 | ent3 | ent4 | ent5 | ent6 |
    enta | entx | equ | hlt | ioc | inc1 | inc2 | inc3 | inc4 | inc5 | inc6 | inca | incx | j1nn | j1np | j1nz | j1n |
    j1p | j1z | j2nn | j2np | j2nz | j2n | j2p | j2z | j3nn | j3np | j3nz | j3n | j3p | j3z | j4nn | j4np | j4nz | j4n |
    j4p | j4z | j5nn | j5np | j5nz | j5n | j5p | j5z | j6nn | j6np | j6nz | j6n | j6p | j6z | jann | janp | janz | jan |
    jap | jaz | je | jge | jg | jle | jl | jmp | jne | jxnn | jxnp | jxnz | jxn | jxp | jxz | ld1n | ld1 | ld2n | ld2 |
    ld3n | ld3 | ld4n | ld4 | ld5n | ld5 | ld6n | ld6 | ldan | lda | ldxn | ldx | num | orig | out | st1 | st2 | st3 |
    st4 | st5 | st6 | sta | stj | stx | stz

  def address: Parser[AddressAST] = "\\S*".r ^^ { s =>
    val lexed = AddressLexer(s).right
    (for {
      tokens <- lexed
      ast <- AddressParser(tokens).right
    } yield ast) match {
      case Right(a) => a
      case _ => Vacuous
    }
  }

  def line: Parser[MixProgramLine] = {
    opt(location) ~ w ~ operation ~ opt(w) ~ opt(address) ~ opt(w) ~ opt(newLine) ^^ {
      case l ~ _ ~ o ~ _ ~ Some(Vacuous) ~ _ ~ _ => MixProgramLine(l, o, None)
      case l ~ _ ~ o ~ _ ~ a ~ _ ~ _ => MixProgramLine(l, o, a)
    }
  }

  def alf: Parser[MixProgramLine] = {
    opt(location) ~ w ~ "ALF  " ~ ".{5}".r ~ opt(w) ~ opt(newLine) ^^ {
      case l ~ _ ~ _ ~ address ~ _ ~ _ => MixProgramLine(l, AlfToken, Some(Symbol(address)))
    }
  }

  def comment: Parser[MixComment] = {
    "*" ~ ".*".r ~ newLine ^^ {
      case _ ~ c ~ _ => MixComment(c)
    }
  }

  def tokens: Parser[List[MixLine]] = {
    phrase(
      rep(
        comment | line | alf
      )
    )
  }

  def apply(code: String): Either[String, List[MixLine]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, _) => Left(s"Error: $msg")
      case Success(result, _) => Right(result)
    }
  }

}
