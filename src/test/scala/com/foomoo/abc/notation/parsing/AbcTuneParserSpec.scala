package com.foomoo.abc.notation.parsing

import java.net.URI

import com.foomoo.abc._

import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

class AbcTuneParserSpec extends UnitSpec {
  val resource: URI = getClass.getResource("/speed_the_plough.abc").toURI
  val multipleTuneResource: URI = getClass.getResource("/multiple_tunes.abc").toURI
  val tuneContents: String = Source.fromFile(resource, "UTF-8").mkString
  val multipleTuneContents: String = Source.fromFile(multipleTuneResource, "UTF-8").mkString

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

  it should "read notes" in {
    assertResult(List(AbcBar(List(AbcNote("c"), AbcNote("e"), AbcNote("c"), AbcNote("C"), AbcNote("d"), AbcNote("B"))))) {
      parsing("c2ec C2dB")(AbcTuneParser.notes)
    }
  }

  it should "read dotted notes" in {
    assertResult(AbcBrokenRythm(AbcNote("A"), AbcNote("B"))) {
      parsing("A>B")(AbcTuneParser.brokenRythm)
    }
  }

  it should "read triplets" in {
    assertResult(AbcTriplet(AbcNote("F"), AbcNote("G"), AbcNote("A"))) {
      parsing("(3F/G/A/")(AbcTuneParser.triplet)
    }
  }

  it should "read lower and uppper octave notes" in {
    assertResult(List(AbcNote("A"), AbcNote("A,"), AbcNote("A,,"), AbcNote("A'"), AbcNote("A''"))) {
      parsing("AA,A,,A'2A''")(AbcTuneParser.rep(AbcTuneParser.note))
    }
  }

  it should "read chords" in {
    assertResult(AbcChord("Em")) {
      parsing("\"Em\"")(AbcTuneParser.chord)
    }
  }

  it should "read bars" in {
    assertResult(List(AbcBar(List(AbcNote("A"), AbcNote("B"))), AbcBar(List(AbcNote("C"), AbcNote("D"))), AbcBar(List(AbcNote("E"), AbcNote("F"))))) {
      parsing("A B|C D|EF")(AbcTuneParser.notes)
    }
  }

  it should "read a bar with chords and notes" in {
    assertResult(AbcBar(List(AbcChord("Em"), AbcNote("A"), AbcNote("B"), AbcChord("Dm")))) {
      parsing("\"Em\"A B \"Dm\"")(AbcTuneParser.bar)
    }
  }

  it should "read single repeat" in {
    assertResult(List(AbcRepeat(List(AbcBar(List(AbcNote("A"))))))) {
      parsing("|:A:|")(AbcTuneParser.notes)
    }
  }

  it should "read repeats" in {
    assertResult(List(AbcRepeat(List(AbcBar(List(AbcNote("A"))), AbcBar(List(AbcNote("B"))))), AbcRepeat(List(AbcBar(List(AbcNote("C"))), AbcBar(List(AbcNote("D"))))))) {
      parsing("|:A|B:||:C|D:|")(AbcTuneParser.notes)
    }
  }

  it should "read a tune" in {
    val abcTune: AbcTune = parsing(tuneContents)(AbcTuneParser.tune)

    abcTune.noteElements should have size 2

    abcTune.noteElements should matchPattern { case AbcRepeat(_) :: AbcRepeat(_) :: Nil => }
  }

  it should "read multiple tunes" in {
    val abcTunes: List[AbcTune] = parsing(multipleTuneContents)(AbcTuneParser.tunes)

    abcTunes should have size 2
  }

  private def parsing[T](s:String)(implicit p:AbcTuneParser.Parser[T]):T = {
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = AbcTuneParser.phrase(p)
    //we need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(s)
    phraseParser(input) match {
      case AbcTuneParser.Success(t,_)     => t
      case AbcTuneParser.NoSuccess(msg, next) => throw new IllegalArgumentException(
        "Could not parse '" + s + "': " + msg + "\nNext is: " + next.pos)
    }
  }
}
