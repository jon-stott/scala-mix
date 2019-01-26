package org.jstott.mix.assembler

import org.jstott.mix.instructions.arithmetic.DIV
import org.jstott.mix.instructions.comparison.CMPA
import org.jstott.mix.instructions.conversion.{CHAR, NUM}
import org.jstott.mix.instructions.io.{IOC, OUT}
import org.jstott.mix.instructions.jump._
import org.jstott.mix.instructions.load._
import org.jstott.mix.instructions.misc.HLT
import org.jstott.mix.{MixByte, Mix, Plus, TwoSignedBytes, Word}
import org.jstott.mix.instructions.store._
import org.jstott.mix.instructions.transfer._

class Assembler(code: String) {

  def assemble: Mix = {
    val finalState = MixParser.apply(code) match {
      case Left(error) => throw new IllegalArgumentException(s"syntax error: $error")
      case Right(lines) =>
        val firstPass = lines.filter(_.isInstanceOf[MixProgramLine]).foldLeft(AssemblerState(Mix())) { case (state, line: MixProgramLine) =>
          handleToken(state, line)
        }
        firstPass.unevaluatedOperations.foldLeft(firstPass) { case (state, (location, line)) =>
          val alteredLocationCounter = state.copy(
            locationCounter = location,
            unevaluatedOperations = state.unevaluatedOperations - location
          )
          val newState = line.operation match {
            case a if a.isInstanceOf[AssemblerToken] => handleAssemblerToken(alteredLocationCounter, line)
            case a if a.isInstanceOf[OperatorToken] => handleOperatorToken(alteredLocationCounter, line)
          }
          newState
        }
    }
    finalState.mix.copy(programCounter = finalState.startExecutionFrom)
  }

  private def handleToken(state: AssemblerState, line: MixProgramLine): AssemblerState = {
    line.operation match {
      case a if a.isInstanceOf[AssemblerToken] => handleAssemblerToken(state, line)
      case a if a.isInstanceOf[OperatorToken] => handleOperatorToken(state, line)
    }
  }

  private def handleAssemblerToken(state: AssemblerState, line: MixProgramLine): AssemblerState = {
    line.operation match {
      case AlfToken =>
        handleAlf(state, line)
      case ConToken =>
        handleCon(state, line)
      case EndToken =>
        handleEnd(state, line)
      case EquToken if line.location.nonEmpty && line.address.nonEmpty =>
        handleEqu(state, line)
      case OrigToken =>
        handleOrig(state, line)
    }
  }

  private def handleOperatorToken(state: AssemblerState, line: MixProgramLine): AssemblerState = {
    line.address match {
      case Some(LiteralExpression(expression)) => handleLiteralOperatorToken(state, line, expression)
      case _                                   => handleNormalOperatorToken(state, line)
    }
  }

  private def handleLiteralOperatorToken(state: AssemblerState, line: MixProgramLine, expression: Expression): AssemblerState = {
    val nextLiteralLocation = state.literalOperations.length + 1
    val nextLiteralLocationStr = s"*$nextLiteralLocation"
    val literalLine = MixProgramLine(Some(LocationToken(nextLiteralLocationStr)), operation = ConToken, address = Some(expression))
    val newState = state.copy(literalOperations = state.literalOperations :+ literalLine)
    val modifiedLine = line.copy(address = Some(ValueExpression(Symbol(nextLiteralLocationStr))))
    handleNormalOperatorToken(newState, modifiedLine)
  }

  private def handleNormalOperatorToken(state: AssemblerState, line: MixProgramLine): AssemblerState = {
    val mixWithSymbolHandled = updateSymbols(state, line)
    val evalAddress = line.address.getOrElse(Vacuous).address(state, line.operation)
    val newState = evalAddress match {
      case Right(EvaluatedAddress(a, i, f)) =>
        val newMix = line.operation match {
          case CharToken => mixWithSymbolHandled.withMemoryUpdated(CHAR.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case CmpaToken => mixWithSymbolHandled.withMemoryUpdated(CMPA.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case Dec1Token => mixWithSymbolHandled.withMemoryUpdated(DEC1.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Dec2Token => mixWithSymbolHandled.withMemoryUpdated(DEC2.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Dec3Token => mixWithSymbolHandled.withMemoryUpdated(DEC3.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Dec4Token => mixWithSymbolHandled.withMemoryUpdated(DEC4.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Dec5Token => mixWithSymbolHandled.withMemoryUpdated(DEC5.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Dec6Token => mixWithSymbolHandled.withMemoryUpdated(DEC6.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case DecaToken => mixWithSymbolHandled.withMemoryUpdated(DECA.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case DecxToken => mixWithSymbolHandled.withMemoryUpdated(DECX.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case DivToken => mixWithSymbolHandled.withMemoryUpdated(DIV.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case Enn1Token => mixWithSymbolHandled.withMemoryUpdated(ENN1.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Enn2Token => mixWithSymbolHandled.withMemoryUpdated(ENN2.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Enn3Token => mixWithSymbolHandled.withMemoryUpdated(ENN3.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Enn4Token => mixWithSymbolHandled.withMemoryUpdated(ENN4.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Enn5Token => mixWithSymbolHandled.withMemoryUpdated(ENN5.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Enn6Token => mixWithSymbolHandled.withMemoryUpdated(ENN6.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case EnnaToken => mixWithSymbolHandled.withMemoryUpdated(ENNA.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case EnnxToken => mixWithSymbolHandled.withMemoryUpdated(ENNX.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ent1Token => mixWithSymbolHandled.withMemoryUpdated(ENT1.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ent2Token => mixWithSymbolHandled.withMemoryUpdated(ENT2.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ent3Token => mixWithSymbolHandled.withMemoryUpdated(ENT3.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ent4Token => mixWithSymbolHandled.withMemoryUpdated(ENT4.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ent5Token => mixWithSymbolHandled.withMemoryUpdated(ENT5.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ent6Token => mixWithSymbolHandled.withMemoryUpdated(ENT6.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case EntaToken => mixWithSymbolHandled.withMemoryUpdated(ENTA.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case EntxToken => mixWithSymbolHandled.withMemoryUpdated(ENTX.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case HltToken => mixWithSymbolHandled.withMemoryUpdated(HLT.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case IocToken => mixWithSymbolHandled.withMemoryUpdated(IOC.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case Inc1Token => mixWithSymbolHandled.withMemoryUpdated(INC1.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Inc2Token => mixWithSymbolHandled.withMemoryUpdated(INC2.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Inc3Token => mixWithSymbolHandled.withMemoryUpdated(INC3.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Inc4Token => mixWithSymbolHandled.withMemoryUpdated(INC4.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Inc5Token => mixWithSymbolHandled.withMemoryUpdated(INC5.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Inc6Token => mixWithSymbolHandled.withMemoryUpdated(INC6.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case IncaToken => mixWithSymbolHandled.withMemoryUpdated(INCA.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case IncxToken => mixWithSymbolHandled.withMemoryUpdated(INCX.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J1nToken => mixWithSymbolHandled.withMemoryUpdated(J1N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J1nnToken => mixWithSymbolHandled.withMemoryUpdated(J1NN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J1npToken => mixWithSymbolHandled.withMemoryUpdated(J1NP.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J1nzToken => mixWithSymbolHandled.withMemoryUpdated(J1NZ.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J1pToken => mixWithSymbolHandled.withMemoryUpdated(J1P.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J1zToken => mixWithSymbolHandled.withMemoryUpdated(J1Z.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J2nToken => mixWithSymbolHandled.withMemoryUpdated(J2N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J2nnToken => mixWithSymbolHandled.withMemoryUpdated(J2NN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J2npToken => mixWithSymbolHandled.withMemoryUpdated(J2NP.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J2nzToken => mixWithSymbolHandled.withMemoryUpdated(J2NZ.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J2pToken => mixWithSymbolHandled.withMemoryUpdated(J2P.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J2zToken => mixWithSymbolHandled.withMemoryUpdated(J2Z.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J3nToken => mixWithSymbolHandled.withMemoryUpdated(J3N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J3nnToken => mixWithSymbolHandled.withMemoryUpdated(J3NN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J3npToken => mixWithSymbolHandled.withMemoryUpdated(J3NP.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J3nzToken => mixWithSymbolHandled.withMemoryUpdated(J3NZ.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J3pToken => mixWithSymbolHandled.withMemoryUpdated(J3P.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J3zToken => mixWithSymbolHandled.withMemoryUpdated(J3Z.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J4nToken => mixWithSymbolHandled.withMemoryUpdated(J4N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J4nnToken => mixWithSymbolHandled.withMemoryUpdated(J4NN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J4npToken => mixWithSymbolHandled.withMemoryUpdated(J4NP.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J4nzToken => mixWithSymbolHandled.withMemoryUpdated(J4NZ.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J4pToken => mixWithSymbolHandled.withMemoryUpdated(J4P.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J4zToken => mixWithSymbolHandled.withMemoryUpdated(J4Z.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J5nToken => mixWithSymbolHandled.withMemoryUpdated(J5N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J5nnToken => mixWithSymbolHandled.withMemoryUpdated(J5NN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J5npToken => mixWithSymbolHandled.withMemoryUpdated(J5NP.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J5nzToken => mixWithSymbolHandled.withMemoryUpdated(J5NZ.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J5pToken => mixWithSymbolHandled.withMemoryUpdated(J5P.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J5zToken => mixWithSymbolHandled.withMemoryUpdated(J5Z.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J6nToken => mixWithSymbolHandled.withMemoryUpdated(J6N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J6nnToken => mixWithSymbolHandled.withMemoryUpdated(J6NN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J6npToken => mixWithSymbolHandled.withMemoryUpdated(J6NP.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J6nzToken => mixWithSymbolHandled.withMemoryUpdated(J6NZ.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J6pToken => mixWithSymbolHandled.withMemoryUpdated(J6P.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case J6zToken => mixWithSymbolHandled.withMemoryUpdated(J6Z.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JanToken => mixWithSymbolHandled.withMemoryUpdated(JAN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JannToken => mixWithSymbolHandled.withMemoryUpdated(JANN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JanpToken => mixWithSymbolHandled.withMemoryUpdated(JANP.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JanzToken => mixWithSymbolHandled.withMemoryUpdated(JANZ.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JapToken => mixWithSymbolHandled.withMemoryUpdated(JAP.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JazToken => mixWithSymbolHandled.withMemoryUpdated(JAZ.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JeToken => mixWithSymbolHandled.withMemoryUpdated(JE.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JgToken => mixWithSymbolHandled.withMemoryUpdated(JG.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JgeToken => mixWithSymbolHandled.withMemoryUpdated(JGE.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JlToken => mixWithSymbolHandled.withMemoryUpdated(JL.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JleToken => mixWithSymbolHandled.withMemoryUpdated(JLE.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JneToken => mixWithSymbolHandled.withMemoryUpdated(JNE.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JmpToken => mixWithSymbolHandled.withMemoryUpdated(JMP.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JxnToken => mixWithSymbolHandled.withMemoryUpdated(JXN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JxnnToken => mixWithSymbolHandled.withMemoryUpdated(JXNN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JxnpToken => mixWithSymbolHandled.withMemoryUpdated(JXNP.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JxnzToken => mixWithSymbolHandled.withMemoryUpdated(JXNZ.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JxpToken => mixWithSymbolHandled.withMemoryUpdated(JXP.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case JxzToken => mixWithSymbolHandled.withMemoryUpdated(JXZ.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld1Token => mixWithSymbolHandled.withMemoryUpdated(LD1.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld1nToken => mixWithSymbolHandled.withMemoryUpdated(LD1N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld2Token => mixWithSymbolHandled.withMemoryUpdated(LD2.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld2nToken => mixWithSymbolHandled.withMemoryUpdated(LD2N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld3Token => mixWithSymbolHandled.withMemoryUpdated(LD3.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld3nToken => mixWithSymbolHandled.withMemoryUpdated(LD3N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld4Token => mixWithSymbolHandled.withMemoryUpdated(LD4.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld4nToken => mixWithSymbolHandled.withMemoryUpdated(LD4N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld5Token => mixWithSymbolHandled.withMemoryUpdated(LD5.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld5nToken => mixWithSymbolHandled.withMemoryUpdated(LD5N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld6Token => mixWithSymbolHandled.withMemoryUpdated(LD6.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case Ld6nToken => mixWithSymbolHandled.withMemoryUpdated(LD6N.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case LdaToken => mixWithSymbolHandled.withMemoryUpdated(LDA.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case LdanToken => mixWithSymbolHandled.withMemoryUpdated(LDAN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case LdxToken => mixWithSymbolHandled.withMemoryUpdated(LDX.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case LdxnToken => mixWithSymbolHandled.withMemoryUpdated(LDXN.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case NumToken => mixWithSymbolHandled.withMemoryUpdated(NUM.apply(TwoSignedBytes(a), MixByte.fromInt(i)).value, state.locationCounter)
          case OutToken => mixWithSymbolHandled.withMemoryUpdated(OUT.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case St1Token => mixWithSymbolHandled.withMemoryUpdated(ST1.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case St2Token => mixWithSymbolHandled.withMemoryUpdated(ST2.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case St3Token => mixWithSymbolHandled.withMemoryUpdated(ST3.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case St4Token => mixWithSymbolHandled.withMemoryUpdated(ST4.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case St5Token => mixWithSymbolHandled.withMemoryUpdated(ST5.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case St6Token => mixWithSymbolHandled.withMemoryUpdated(ST6.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case StaToken => mixWithSymbolHandled.withMemoryUpdated(STA.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case StjToken => mixWithSymbolHandled.withMemoryUpdated(STJ.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case StxToken => mixWithSymbolHandled.withMemoryUpdated(STX.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case StzToken => mixWithSymbolHandled.withMemoryUpdated(STZ.apply(TwoSignedBytes(a), MixByte.fromInt(i), MixByte.fromInt(f)).value, state.locationCounter)
          case _ => mixWithSymbolHandled
        }
        state.copy(
          mix = newMix,
          locationCounter = state.locationCounter + 1
        )
      case _ =>
        state.copy(
          mix = mixWithSymbolHandled,
          unevaluatedOperations = state.unevaluatedOperations + (state.locationCounter -> line),
          locationCounter = state.locationCounter + 1
        )
    }
    newState
  }

  private def handleAlf(state: AssemblerState, line: MixProgramLine): AssemblerState = {
    val mixWithSymbolHandled = updateSymbols(state, line)
    line.address match {
      case Some(Symbol(s)) if s.length == 5 =>
        val word = Word(
          Plus,
          Seq(
            MixByte.fromCharacter(s.head),
            MixByte.fromCharacter(s(1)),
            MixByte.fromCharacter(s(2)),
            MixByte.fromCharacter(s(3)),
            MixByte.fromCharacter(s(4))
          )
        )
        val newMix = mixWithSymbolHandled.withMemoryUpdated(word, state.locationCounter)
        state.copy(mix = newMix, locationCounter = state.locationCounter + 1)
      case _ => handleCon(state.copy(mix = mixWithSymbolHandled), line)
    }
  }

  private def handleCon(state: AssemblerState, line: MixProgramLine): AssemblerState = {
    val mixWithSymbolHandled = updateSymbols(state, line)
    val add = line.address.getOrElse(Vacuous).evaluate(state) match {
      case Left(e) => throw new IllegalArgumentException(s"Can't evaluate: $e")
      case Right(v) => v
    }
    val newMix = mixWithSymbolHandled.withMemoryUpdated(Word(add), state.locationCounter)
    state.copy(mix = newMix, locationCounter = state.locationCounter + 1)
  }

  private def handleEnd(originalState: AssemblerState, line: MixProgramLine): AssemblerState = {
    val updatedState = originalState.literalOperations.foldLeft(originalState) { case (state, line: MixProgramLine) =>
      handleToken(state, line)
    }
    val updatedStateWithHlt = handleToken(updatedState, MixProgramLine(None, HltToken, None))
    updatedStateWithHlt.copy(
      startExecutionFrom = line.address match {
        case Some(x) => x.evaluate(updatedState) match {
          case Left(_) => 0
          case Right(address) => address
        }
        case _ => 0
      }
    )
  }

  private def handleEqu(state: AssemblerState, line: MixProgramLine): AssemblerState = {
    val loc = line.location.map(_.s).getOrElse("")
    val add = line.address.getOrElse(Vacuous).evaluate(state) match {
      case Left(_) => Left(line.address.getOrElse(Vacuous))
      case Right(v) => Right(v)
    }
    val newMix = state.mix.copy(symbols = state.mix.symbols + (loc -> add))
    state.copy(mix = newMix)
  }

  private def handleOrig(state: AssemblerState, line: MixProgramLine): AssemblerState = {
    val add = line.address.getOrElse(Vacuous).evaluate(state) match {
      case Left(e) => throw new IllegalArgumentException(s"Can't evaluate: $e")
      case Right(v) => v
    }
    state.copy(locationCounter = add)
  }

  private def updateSymbols(state: AssemblerState, line: MixProgramLine): Mix = {
    line.location match {
      case Some(LocationToken(Symbol.localSymbolPattern(n, "H"))) =>
        val updated = state.mix.localSymbols.get(n.toInt) match {
          case Some(existingLocalSymbol) => existingLocalSymbol + state.locationCounter
          case None => Set(state.locationCounter)
        }
        state.mix.copy(localSymbols = state.mix.localSymbols + (n.toInt -> updated))
      case Some(s) =>
        state.mix.copy(symbols = state.mix.symbols + (s.s -> Right(state.locationCounter)))
      case None =>
        state.mix
    }
  }

}
