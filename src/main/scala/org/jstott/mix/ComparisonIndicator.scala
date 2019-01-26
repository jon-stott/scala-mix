package org.jstott.mix

sealed trait ComparisonIndicator

case object Less extends ComparisonIndicator

case object Equal extends ComparisonIndicator

case object Greater extends ComparisonIndicator