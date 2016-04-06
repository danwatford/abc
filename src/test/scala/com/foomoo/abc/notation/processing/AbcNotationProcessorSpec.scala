package com.foomoo.abc.notation.processing

import com.foomoo.abc._
import com.foomoo.abc.notation.AbcNotationTestSupport._
import com.foomoo.abc.notation._

class AbcNotationProcessorSpec extends UnitSpec {

  "The AbcNotationProcessor" should "strip comments from the tune notation header" in {
    val normalisedTune = AbcNotationProcessor.normalise(TEST_TUNE)

    assertResult(false) {
      normalisedTune.header.lines exists {
        case headerLine: AbcNotationHeaderLineComment => true
        case _ => false
      }
    }
  }

  it should "strip inline comments from the tune notation body" in {
    val normalisedTune = AbcNotationProcessor.normalise(TEST_TUNE)
    val strippedElements: List[AbcNotationBodyElement] = normalisedTune.body.elements

    assertResult(false) {
      strippedElements exists {
        case comment: AbcNotationBodyComment => true
        case _ => false
      }
    }
  }

  it should "join header information fields" in {
    val normalisedTune = AbcNotationProcessor.normalise(TEST_TUNE)

    val fieldValues: List[String] = headerValue(normalisedTune, "A")
    assertResult(1)(fieldValues.size)
    assertResult("Part1Part2")(fieldValues.head)
  }

  it should "join tune notation body lines" in {
    val normalisedTune = AbcNotationProcessor.normalise(TEST_TUNE)
    val joinedElements: List[AbcNotationBodyElement] = normalisedTune.body.elements

    assertResult(true)(joinedElements.containsSlice(List(NOTE_C, NOTE_D)))
  }

  it should "extract a tune's note sequence" in {
    val expectedNotes: Seq[AbcNotationNote] = Seq(NOTE_A, NOTE_B, NOTE_C, NOTE_D)

    assertResult(expectedNotes) {
      AbcNotationProcessor.simpleNoteExtract(TEST_TUNE)
    }
  }

}
