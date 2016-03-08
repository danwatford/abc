package com.foomoo.abc.notation.processing

import com.foomoo.abc._
import com.foomoo.abc.notation.AbcNotationTestSupport._
import com.foomoo.abc.notation._

class AbcNotationProcessorSpec extends UnitSpec {

  val testHeaderLines = List(
    AbcNotationHeaderInformationField('X', "1"),
    AbcNotationHeaderLineComment("Test Comment"),
    AbcNotationHeaderInformationField('T', "Test Tune Title"),
    AbcNotationHeaderInformationField('A', "Part1"),
    AbcNotationHeaderLineComment("Test Comment"),
    AbcNotationHeaderInformationField('+', "Part2"),
    AbcNotationHeaderInformationField('K', "C")
  )
  val testHeader = AbcNotationHeader(testHeaderLines)

  val testBodyLine1 = List(
    NOTE_A,
    NOTE_B,
    AbcBodyCommentNotation("Inline comment 1"),
    AbcBodyInformationFieldNotation('K', "D"),
    AbcBodyCommentNotation("Inline comment 2"),
    AbcBarNotation("|"),
    BODY_NEWLINE
  )
  val testBodyLine2 = List(AbcBodyInformationFieldNotation('M', "1"))
  val testBodyLine3 = List(AbcBodyCommentNotation("Line comment 1"))
  val testBodyLine4 = List(
    NOTE_C,
    BODY_CONTINUATION,
    BODY_NEWLINE
  )
  val testBodyLine5 = List(AbcBodyCommentNotation("Line comment 2"))
  val testBodyLine6 = List(
    NOTE_D,
    BODY_NEWLINE
  )

  val testBody = AbcNotationBody(List(testBodyLine1, testBodyLine2, testBodyLine3, testBodyLine4, testBodyLine5, testBodyLine6).flatten)

  val testTune = AbcTuneNotation(testHeader, testBody)


  "The AbcNotationProcessor" should "strip comments from the tune notation header" in {
    val normalisedTune = AbcNotationProcessor.normalise(testTune)

    assertResult(false) {
      normalisedTune.header.lines exists {
        case headerLine: AbcNotationHeaderLineComment => true
        case _ => false
      }
    }
  }

  it should "strip inline comments from the tune notation body" in {
    val normalisedTune = AbcNotationProcessor.normalise(testTune)
    val strippedElements: List[AbcNotationBodyElement] = normalisedTune.body.elements

    assertResult(false) {
      strippedElements exists {
        case comment: AbcBodyCommentNotation => true
        case _ => false
      }
    }
  }

  it should "join header information fields" in {
    val normalisedTune = AbcNotationProcessor.normalise(testTune)

    val fieldValues: List[String] = headerValue(normalisedTune, 'A')
    assertResult(1)(fieldValues.size)
    assertResult("Part1Part2")(fieldValues.head)
  }

  it should "join tune notation body lines" in {
    val normalisedTune = AbcNotationProcessor.normalise(testTune)
    val joinedElements: List[AbcNotationBodyElement] = normalisedTune.body.elements

    assertResult(true)(joinedElements.containsSlice(List(NOTE_C, NOTE_D)))
  }

}
