package com.foomoo.abc.notation

/**
  * Contains common items to support testing.
  */
object AbcNotationTestSupport {

  val NOTE_A = AbcNotationNote("A")
  val NOTE_B = AbcNotationNote("B")
  val NOTE_C = AbcNotationNote("C")
  val NOTE_D = AbcNotationNote("D")
  val NOTE_E = AbcNotationNote("E")
  val NOTE_F = AbcNotationNote("F")
  val NOTE_G = AbcNotationNote("G")

  val NOTE_a = AbcNotationNote("a")
  val NOTE_b = AbcNotationNote("b")
  val NOTE_c = AbcNotationNote("c")
  val NOTE_d = AbcNotationNote("d")
  val NOTE_e = AbcNotationNote("e")
  val NOTE_f = AbcNotationNote("f")
  val NOTE_g = AbcNotationNote("g")

  val REST_Z = AbcNotationRest("Z")
  val REST_X = AbcNotationRest("X")
  val REST_z = AbcNotationRest("z")
  val REST_x = AbcNotationRest("x")

  val WHITESPACE = AbcNotationBodyWhitespace(" ")
  val BODY_NEWLINE = AbcNotationBodyNewLine()
  val BODY_CONTINUATION = AbcNotationBodyLineContinuation()

  val SCORE_LINEBREAK = AbcNotationBodyScoreLineBreak()

  val TEST_HEADER_LINES = List(
    AbcNotationHeaderInformationField("X", "1"),
    AbcNotationHeaderLineComment("Test Comment"),
    AbcNotationHeaderInformationField("T", "Test Tune Title"),
    AbcNotationHeaderInformationField("A", "Part1"),
    AbcNotationHeaderLineComment("Test Comment"),
    AbcNotationHeaderInformationField("+", "Part2"),
    AbcNotationHeaderInformationField("K", "C")
  )
  val TEST_HEADER = AbcNotationHeader(TEST_HEADER_LINES)

  val TEST_BODY_LINE_1 = List(
    NOTE_A,
    NOTE_B,
    AbcNotationBodyComment("Inline comment 1"),
    AbcNotationBodyInformationField("K", "D"),
    AbcNotationBodyComment("Inline comment 2"),
    AbcNotationBar("|"),
    BODY_NEWLINE
  )
  val TEST_BODY_LINE_2 = List(AbcNotationBodyInformationField("M", "1"))
  val TEST_BODY_LINE_3 = List(AbcNotationBodyComment("Line comment 1"))
  val TEST_BODY_LINE_4 = List(
    NOTE_C,
    BODY_CONTINUATION,
    BODY_NEWLINE
  )
  val TEST_BODY_LINE_5 = List(AbcNotationBodyComment("Line comment 2"))
  val TEST_BODY_LINE_6 = List(
    NOTE_D,
    BODY_NEWLINE
  )

  val TEST_BODY = AbcNotationBody(List(TEST_BODY_LINE_1, TEST_BODY_LINE_2, TEST_BODY_LINE_3, TEST_BODY_LINE_4, TEST_BODY_LINE_5, TEST_BODY_LINE_6).flatten)

  val TEST_TUNE = AbcNotationTune(TEST_HEADER, TEST_BODY)

  def headerValue(tuneNotation: AbcNotationTune, key: String): List[String] = tuneNotation match {
    case AbcNotationTune(AbcNotationHeader(headerList), _) =>
      headerList.filter {
        case AbcNotationHeaderInformationField(headerKey, _) => headerKey == key
        case _ => false
      } map {
        case AbcNotationHeaderInformationField(_, headerValue) => headerValue
        case _ => throw new IllegalArgumentException("Only AbcNotationHeaderInformation expected")
      }
  }

}
