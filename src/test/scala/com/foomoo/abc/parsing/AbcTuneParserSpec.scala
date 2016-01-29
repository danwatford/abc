package com.foomoo.abc.parsing

import java.net.URI

import com.foomoo.abc.{AbcTune, UnitSpec}

import scala.io.Source

class AbcTuneParserSpec extends UnitSpec {
  val resource: URI = getClass.getResource("/speed_the_plough.abc").toURI
  val tuneContents: String = Source.fromFile(resource, "UTF-8").mkString

  "The parser" should "read a tune's title" in {

    val abcTune: AbcTune = AbcTuneParser.parseAbcTune(tuneContents)
    assertResult(Some("Speed the Plough")) {
      abcTune.title
    }
  }

  it should "read a tune's reference" in {
    val abcTune: AbcTune = AbcTuneParser.parseAbcTune(tuneContents)
    assertResult(Some("1")) {
      abcTune.reference
    }
  }

  it should "read a tune's meter" in {
    val abcTune: AbcTune = AbcTuneParser.parseAbcTune(tuneContents)
    assertResult(Some("4/4")) {
      abcTune.meter
    }
  }

  it should "read a tune's key" in {
    val abcTune: AbcTune = AbcTuneParser.parseAbcTune(tuneContents)
    assertResult("G") {
      abcTune.key
    }
  }

  it should "read a tune's composer" in {
    val abcTune: AbcTune = AbcTuneParser.parseAbcTune(tuneContents)
    assertResult(Some("Trad.")) {
      abcTune.composer
    }
  }

}
