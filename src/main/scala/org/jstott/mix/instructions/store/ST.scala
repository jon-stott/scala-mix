package org.jstott.mix.instructions.store

import org.jstott.mix.instructions.Instruction
import org.jstott.mix.{Mix, Word}

trait ST extends Instruction {

  protected def computeStore(mix: Mix, originalWord: Word): Word = {
    val field = F.asField
    val wordInMemory = mix.contents(A, I)
    val sign = if (field.left == 0) originalWord.sign else wordInMemory.sign
    val numBytesToTake = (field.right + 1) - (if (field.left == 0) field.left + 1 else field.left)
    val bytesToCopy = originalWord.bytes.takeRight(numBytesToTake)
    val index = if (field.left == 0) 1 else field.left
    wordInMemory.withSign(sign).withBytes(bytesToCopy, index)
  }

}
