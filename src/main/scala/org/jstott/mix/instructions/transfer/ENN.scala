package org.jstott.mix.instructions.transfer

import org.jstott.mix.{Mix, Word}

trait ENN extends ENT {

  override def computeValue(mix: Mix): Word = {
    super.computeValue(mix).invertSign
  }

}
