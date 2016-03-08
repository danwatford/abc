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

  def headerValue(tuneNotation: AbcTuneNotation, key: Char): List[String] = tuneNotation match {
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
