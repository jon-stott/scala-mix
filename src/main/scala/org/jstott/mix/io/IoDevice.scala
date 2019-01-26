package org.jstott.mix.io

import org.jstott.mix.Mix

trait IoDevice {

  def ioc(mix: Mix): Unit

  def out(mix: Mix, address: Int): Unit

}
