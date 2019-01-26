package org.jstott.mix.assembler

import org.jstott.mix.Mix

case class AssemblerState(
                           mix: Mix,
                           locationCounter: Int = 0,
                           unevaluatedOperations: Map[Int, MixProgramLine] = Map.empty,
                           literalOperations: Seq[MixProgramLine] = Seq.empty,
                           startExecutionFrom: Int = 0
                         )
