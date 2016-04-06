package com.foomoo.abc.tune.conversion

import com.foomoo.abc.UnitSpec
import com.foomoo.abc.notation._
import com.foomoo.abc.tune._

/**
  * Tests for the AbcNotationConverter.
  */
class AbcNotationConverterSpec extends UnitSpec {

  import com.foomoo.abc.notation.AbcNotationTestSupport._

  val MINIMAL_TUNE_HEADER_NOTATION = AbcNotationHeader(List(AbcNotationHeaderInformationField("X", "1"), AbcNotationHeaderInformationField("K", "C")))


  "AbcNotationConverter" should "read notes into a bar structure" in {

    val testBody = AbcNotationBody(List(NOTE_A, NOTE_B, NOTE_C))

    inside(AbcNotationConverter.convertBody(testBody)) {
      case AbcBar(barNoteElements) :: Nil =>
        barNoteElements should contain allOf(AbcNote("A"), AbcNote("B"), AbcNote("C"))
    }
  }

  it should "include file header information field in the tune" in {

    val fileHeader: AbcNotationFileHeader = AbcNotationFileHeader(List(AbcNotationHeaderInformationField("M", "4/4")))

    val tuneNotation: AbcNotationTune = AbcNotationTune(MINIMAL_TUNE_HEADER_NOTATION, AbcNotationBody(List(NOTE_A)))

    val tune: AbcTune = AbcNotationConverter.convertTune(tuneNotation, fileHeader)
    tune.meter.value should be ("4/4")
    tune.key should be ("C")
  }

  it should "detect bars containing chords" in {
    val tuneNotation: AbcNotationTune = AbcNotationTune(MINIMAL_TUNE_HEADER_NOTATION, AbcNotationBody(List(AbcNotationChord("Dm"), NOTE_A)))

    val tune: AbcTune = AbcNotationConverter.convertTune(tuneNotation)
    inside(tune.bodyElements) {
      case AbcBar(AbcChord("Dm") :: _) :: Nil => ()
    }
  }

  it should "ignore broken rythms but still convert the notes" in {
    val tuneNotation: AbcNotationTune = AbcNotationTune(MINIMAL_TUNE_HEADER_NOTATION, AbcNotationBody(List(NOTE_A, AbcNotationBrokenRythm("<"), NOTE_B)))

    val tune: AbcTune = AbcNotationConverter.convertTune(tuneNotation)
    inside(tune.bodyElements) {
      case AbcBar(AbcNote("A") :: AbcNote("B") :: Nil) :: Nil => ()
    }
  }

  it should "ignore ties but still convert the notes" in {
    val tuneNotation: AbcNotationTune = AbcNotationTune(MINIMAL_TUNE_HEADER_NOTATION, AbcNotationBody(List(NOTE_A, AbcNotationTie(), NOTE_B)))

    val tune: AbcTune = AbcNotationConverter.convertTune(tuneNotation)
    inside(tune.bodyElements) {
      case AbcBar(AbcNote("A") :: AbcNote("B") :: Nil) :: Nil => ()
    }
  }

  it should "ignore slurs but still convert the notes" in {
    val tuneNotation: AbcNotationTune = AbcNotationTune(MINIMAL_TUNE_HEADER_NOTATION, AbcNotationBody(List(AbcNotationSlurStart(), NOTE_A, NOTE_B, AbcNotationSlurEnd())))

    val tune: AbcTune = AbcNotationConverter.convertTune(tuneNotation)
    inside(tune.bodyElements) {
      case AbcBar(AbcNote("A") :: AbcNote("B") :: Nil) :: Nil => ()
    }
  }

  it should "ignore grace notes" in {
    val tuneNotation: AbcNotationTune = AbcNotationTune(MINIMAL_TUNE_HEADER_NOTATION, AbcNotationBody(List(NOTE_A, AbcNotationGraceStart(), NOTE_B, NOTE_C, AbcNotationGraceEnd(), NOTE_D)))

    val tune: AbcTune = AbcNotationConverter.convertTune(tuneNotation)
    inside(tune.bodyElements) {
      case AbcBar(AbcNote("A") :: AbcNote("D") :: Nil) :: Nil => ()
    }
  }

  it should "convert unison notes" in {
    val tuneNotation: AbcNotationTune = AbcNotationTune(MINIMAL_TUNE_HEADER_NOTATION, AbcNotationBody(List(AbcNotationUnisonStart(), NOTE_A, NOTE_B, AbcNotationUnisonEnd())))

    val tune: AbcTune = AbcNotationConverter.convertTune(tuneNotation)
    inside(tune.bodyElements) {
      case AbcBar(AbcUnison(AbcNote("A") :: AbcNote("B") :: Nil) :: Nil) :: Nil => ()
    }
  }

}
