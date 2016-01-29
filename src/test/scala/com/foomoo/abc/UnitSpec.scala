package com.foomoo.abc

import org.scalatest._

abstract trait UnitSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors

