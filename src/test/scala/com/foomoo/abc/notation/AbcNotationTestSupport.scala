package com.foomoo.abc.notation

/**
  * Contains common items to support testing.
  */
object AbcNotationTestSupport {

  val NOTE_A = AbcNoteNotation("A")
  val NOTE_B = AbcNoteNotation("B")
  val NOTE_C = AbcNoteNotation("C")
  val NOTE_D = AbcNoteNotation("D")
  val NOTE_E = AbcNoteNotation("E")
  val NOTE_F = AbcNoteNotation("E")
  val NOTE_G = AbcNoteNotation("E")

  val NOTE_a = AbcNoteNotation("a")
  val NOTE_b = AbcNoteNotation("b")
  val NOTE_c = AbcNoteNotation("c")
  val NOTE_d = AbcNoteNotation("d")
  val NOTE_e = AbcNoteNotation("e")
  val NOTE_f = AbcNoteNotation("f")
  val NOTE_g = AbcNoteNotation("g")

  val REST_Z = AbcRestNotation("Z")
  val REST_X = AbcRestNotation("X")
  val REST_z = AbcRestNotation("z")
  val REST_x = AbcRestNotation("x")

  val WHITESPACE = AbcBodyWhitespaceNotation(" ")
  val BODY_NEWLINE = AbcBodyNewLine()
  val BODY_CONTINUATION = AbcBodyLineContinuation()

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
    AbcBodyCommentNotation("Inline comment 1"),
    AbcBodyInformationFieldNotation("K", "D"),
    AbcBodyCommentNotation("Inline comment 2"),
    AbcBarNotation("|"),
    BODY_NEWLINE
  )
  val TEST_BODY_LINE_2 = List(AbcBodyInformationFieldNotation("M", "1"))
  val TEST_BODY_LINE_3 = List(AbcBodyCommentNotation("Line comment 1"))
  val TEST_BODY_LINE_4 = List(
    NOTE_C,
    BODY_CONTINUATION,
    BODY_NEWLINE
  )
  val TEST_BODY_LINE_5 = List(AbcBodyCommentNotation("Line comment 2"))
  val TEST_BODY_LINE_6 = List(
    NOTE_D,
    BODY_NEWLINE
  )

  val TEST_BODY = AbcNotationBody(List(TEST_BODY_LINE_1, TEST_BODY_LINE_2, TEST_BODY_LINE_3, TEST_BODY_LINE_4, TEST_BODY_LINE_5, TEST_BODY_LINE_6).flatten)

  val TEST_TUNE = AbcTuneNotation(TEST_HEADER, TEST_BODY)

  def headerValue(tuneNotation: AbcTuneNotation, key: String): List[String] = tuneNotation match {
    case AbcTuneNotation(AbcNotationHeader(headerList), _) =>
      headerList.filter {
        case AbcNotationHeaderInformationField(headerKey, _) => headerKey == key
        case _ => false
      } map {
        case AbcNotationHeaderInformationField(_, headerValue) => headerValue
        case _ => throw new IllegalArgumentException("Only AbcNotationHeaderInformation expected")
      }
  }

}
