package com.foomoo.abc.tune.conversion

import com.foomoo.abc.UnitSpec
import com.foomoo.abc.notation._
import com.foomoo.abc.tune._

import scala.collection.immutable.::

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

  it should "read a repeated section" in {
    val testBody = AbcNotationBody(List(AbcNotationRepeat("|:"), NOTE_A, NOTE_B, AbcNotationRepeat(":|")))

    inside(AbcNotationConverter.convertBody(testBody)) {
      case AbcRepeat(AbcBar(barNoteElements) :: Nil) :: Nil =>
        barNoteElements should contain allOf(AbcNote("A"), AbcNote("B"))
    }
  }

  it should "read a repeated section where the start repeat marker is missing" in {
    val testBody = AbcNotationBody(List(NOTE_A, NOTE_B, AbcNotationRepeat(":|")))

    inside(AbcNotationConverter.convertBody(testBody)) {
      case AbcRepeat(AbcBar(barNoteElements) :: Nil) :: Nil =>
        barNoteElements should contain allOf(AbcNote("A"), AbcNote("B"))
    }
  }

  it should "read a repeated section where the end repeat marker is missing" in {
    val testBody = AbcNotationBody(List(AbcNotationRepeat("|:"), NOTE_A, NOTE_B))

    inside(AbcNotationConverter.convertBody(testBody)) {
      case AbcRepeat(AbcBar(barNoteElements) :: Nil) :: Nil =>
        barNoteElements should contain allOf(AbcNote("A"), AbcNote("B"))
    }
  }

  it should "read multiple repeated sections" in {
    val testBody = AbcNotationBody(List(AbcNotationRepeat("|:"), NOTE_A, NOTE_B, AbcNotationRepeat(":|"),
      AbcNotationRepeat("|:"), NOTE_C, NOTE_D, AbcNotationRepeat(":|")))

    inside(AbcNotationConverter.convertBody(testBody)) {
      case AbcRepeat(AbcBar(bar1NoteElements) :: Nil) :: AbcRepeat(AbcBar(bar2NoteElements) :: Nil) :: Nil =>
        bar1NoteElements should contain allOf(AbcNote("A"), AbcNote("B"))
        bar2NoteElements should contain allOf(AbcNote("C"), AbcNote("D"))
    }
  }

  it should "read multiple repeated sections where first start marker and last end marker are missing" in {
    val testBody = AbcNotationBody(List(NOTE_A, NOTE_B, AbcNotationRepeat(":|"),
      AbcNotationRepeat("|:"), NOTE_C, NOTE_D))

    inside(AbcNotationConverter.convertBody(testBody)) {
      case AbcRepeat(AbcBar(bar1NoteElements) :: Nil) :: AbcRepeat(AbcBar(bar2NoteElements) :: Nil) :: Nil =>
        bar1NoteElements should contain allOf(AbcNote("A"), AbcNote("B"))
        bar2NoteElements should contain allOf(AbcNote("C"), AbcNote("D"))
    }
  }

  it should "read multiple repeated sections using merged start and end repeat markers" in {
    val testBody = AbcNotationBody(List(NOTE_A, NOTE_B, AbcNotationRepeat(":||:"), NOTE_C, NOTE_D))

    inside(AbcNotationConverter.convertBody(testBody)) {
      case AbcRepeat(AbcBar(bar1NoteElements) :: Nil) :: AbcRepeat(AbcBar(bar2NoteElements) :: Nil) :: Nil =>
        bar1NoteElements should contain allOf(AbcNote("A"), AbcNote("B"))
        bar2NoteElements should contain allOf(AbcNote("C"), AbcNote("D"))
    }
  }

  it should "read numbered repeat sections" in {
    val testBody = AbcNotationBody(List(NOTE_A, AbcNotationNumberedRepeat(1), NOTE_B, AbcNotationNumberedRepeat(2), NOTE_C, AbcNotationRepeat(":|")))

    inside(AbcNotationConverter.convertBody(testBody)) {
      case AbcNumberedRepeat(commonSection, numberedSectionsMap) :: Nil =>
        commonSection match {
          case AbcBar(AbcNote("A") :: Nil) :: Nil => ()
        }
        numberedSectionsMap should not contain key(0)
        numberedSectionsMap should contain key 1
        numberedSectionsMap should contain key 2
        numberedSectionsMap(1) match {
          case AbcBar(AbcNote("B") :: Nil) :: Nil => ()
        }
        numberedSectionsMap(2) match {
          case AbcBar(AbcNote("C") :: Nil) :: Nil => ()
        }
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
