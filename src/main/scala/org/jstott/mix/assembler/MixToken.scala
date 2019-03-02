package org.jstott.mix.assembler

import org.jstott.mix.Field

sealed trait MixAssemblerToken

case object WhitespaceToken extends MixAssemblerToken
case object NewLineToken extends MixAssemblerToken

case class LocationToken(s: String) extends MixAssemblerToken

sealed trait OperationToken extends MixAssemblerToken {
  def defaultField: Field = Field.normal
}

sealed trait AssemblerToken extends OperationToken
sealed trait OperatorToken extends OperationToken

case object AlfToken extends AssemblerToken
case object ConToken extends AssemblerToken
case object EndToken extends AssemblerToken
case object EquToken extends AssemblerToken
case object OrigToken extends AssemblerToken

case object CharToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object CmpaToken extends OperatorToken
case object Cmp1Token extends OperatorToken
case object Cmp2Token extends OperatorToken
case object Cmp3Token extends OperatorToken
case object Cmp4Token extends OperatorToken
case object Cmp5Token extends OperatorToken
case object Cmp6Token extends OperatorToken
case object CmpxToken extends OperatorToken
case object Dec1Token extends OperatorToken { override def defaultField = Field(0, 1) }
case object Dec2Token extends OperatorToken { override def defaultField = Field(0, 1) }
case object Dec3Token extends OperatorToken { override def defaultField = Field(0, 1) }
case object Dec4Token extends OperatorToken { override def defaultField = Field(0, 1) }
case object Dec5Token extends OperatorToken { override def defaultField = Field(0, 1) }
case object Dec6Token extends OperatorToken { override def defaultField = Field(0, 1) }
case object DecaToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object DecxToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object DivToken extends OperatorToken
case object Enn1Token extends OperatorToken { override def defaultField = Field(0, 3) }
case object Enn2Token extends OperatorToken { override def defaultField = Field(0, 3) }
case object Enn3Token extends OperatorToken { override def defaultField = Field(0, 3) }
case object Enn4Token extends OperatorToken { override def defaultField = Field(0, 3) }
case object Enn5Token extends OperatorToken { override def defaultField = Field(0, 3) }
case object Enn6Token extends OperatorToken { override def defaultField = Field(0, 3) }
case object EnnaToken extends OperatorToken { override def defaultField = Field(0, 3) }
case object EnnxToken extends OperatorToken { override def defaultField = Field(0, 3) }
case object Ent1Token extends OperatorToken { override def defaultField = Field(0, 2) }
case object Ent2Token extends OperatorToken { override def defaultField = Field(0, 2) }
case object Ent3Token extends OperatorToken { override def defaultField = Field(0, 2) }
case object Ent4Token extends OperatorToken { override def defaultField = Field(0, 2) }
case object Ent5Token extends OperatorToken { override def defaultField = Field(0, 2) }
case object Ent6Token extends OperatorToken { override def defaultField = Field(0, 2) }
case object EntaToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object EntxToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object HltToken extends OperatorToken
case object Inc1Token extends OperatorToken { override def defaultField = Field(0, 0) }
case object Inc2Token extends OperatorToken { override def defaultField = Field(0, 0) }
case object Inc3Token extends OperatorToken { override def defaultField = Field(0, 0) }
case object Inc4Token extends OperatorToken { override def defaultField = Field(0, 0) }
case object Inc5Token extends OperatorToken { override def defaultField = Field(0, 0) }
case object Inc6Token extends OperatorToken { override def defaultField = Field(0, 0) }
case object IncaToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object IncxToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object IocToken extends OperatorToken
case object J1nToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object J1nnToken extends OperatorToken { override def defaultField = Field(0, 3) }
case object J1npToken extends OperatorToken { override def defaultField = Field(0, 5) }
case object J1nzToken extends OperatorToken { override def defaultField = Field(0, 4) }
case object J1pToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object J1zToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object J2nToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object J2nnToken extends OperatorToken { override def defaultField = Field(0, 3) }
case object J2npToken extends OperatorToken { override def defaultField = Field(0, 5) }
case object J2nzToken extends OperatorToken { override def defaultField = Field(0, 4) }
case object J2pToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object J2zToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object J3nToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object J3nnToken extends OperatorToken { override def defaultField = Field(0, 3) }
case object J3npToken extends OperatorToken { override def defaultField = Field(0, 5) }
case object J3nzToken extends OperatorToken { override def defaultField = Field(0, 4) }
case object J3pToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object J3zToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object J4nToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object J4nnToken extends OperatorToken { override def defaultField = Field(0, 3) }
case object J4npToken extends OperatorToken { override def defaultField = Field(0, 5) }
case object J4nzToken extends OperatorToken { override def defaultField = Field(0, 4) }
case object J4pToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object J4zToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object J5nToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object J5nnToken extends OperatorToken { override def defaultField = Field(0, 3) }
case object J5npToken extends OperatorToken { override def defaultField = Field(0, 5) }
case object J5nzToken extends OperatorToken { override def defaultField = Field(0, 4) }
case object J5pToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object J5zToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object J6nToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object J6nnToken extends OperatorToken { override def defaultField = Field(0, 3) }
case object J6npToken extends OperatorToken { override def defaultField = Field(0, 5) }
case object J6nzToken extends OperatorToken { override def defaultField = Field(0, 4) }
case object J6pToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object J6zToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object JanToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object JannToken extends OperatorToken { override def defaultField = Field(0, 3) }
case object JanpToken extends OperatorToken { override def defaultField = Field(0, 5) }
case object JanzToken extends OperatorToken { override def defaultField = Field(0, 4) }
case object JapToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object JazToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object JxnToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object JxnnToken extends OperatorToken { override def defaultField = Field(0, 3) }
case object JxnpToken extends OperatorToken { override def defaultField = Field(0, 5) }
case object JxnzToken extends OperatorToken { override def defaultField = Field(0, 4) }
case object JxpToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object JxzToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object JeToken extends OperatorToken { override def defaultField = Field(0, 5) }
case object JgToken extends OperatorToken { override def defaultField = Field(0, 6) }
case object JgeToken extends OperatorToken { override def defaultField = Field(0, 7) }
case object JlToken extends OperatorToken { override def defaultField = Field(0, 4) }
case object JleToken extends OperatorToken { override def defaultField = Field(1, 1) }
case object JmpToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object JneToken extends OperatorToken { override def defaultField = Field(1, 0) }
case object JnovToken extends OperatorToken { override def defaultField = Field(0, 3) }
case object JovToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object JsjToken extends OperatorToken { override def defaultField = Field(0, 1) }
case object Ld1Token extends OperatorToken
case object Ld1nToken extends OperatorToken
case object Ld2Token extends OperatorToken
case object Ld2nToken extends OperatorToken
case object Ld3Token extends OperatorToken
case object Ld3nToken extends OperatorToken
case object Ld4Token extends OperatorToken
case object Ld4nToken extends OperatorToken
case object Ld5Token extends OperatorToken
case object Ld5nToken extends OperatorToken
case object Ld6Token extends OperatorToken
case object Ld6nToken extends OperatorToken
case object LdaToken extends OperatorToken
case object LdanToken extends OperatorToken
case object LdxToken extends OperatorToken
case object LdxnToken extends OperatorToken
case object NumToken extends OperatorToken { override def defaultField = Field(0, 0) }
case object OutToken extends OperatorToken
case object St1Token extends OperatorToken
case object St2Token extends OperatorToken
case object St3Token extends OperatorToken
case object St4Token extends OperatorToken
case object St5Token extends OperatorToken
case object St6Token extends OperatorToken
case object StaToken extends OperatorToken
case object StjToken extends OperatorToken { override def defaultField = Field(0, 2) }
case object StxToken extends OperatorToken
case object StzToken extends OperatorToken

sealed trait MixLine

case class MixProgramLine(location: Option[LocationToken], operation: OperationToken, address: Option[AddressAST]) extends MixLine

case class MixComment(comment: String) extends MixLine