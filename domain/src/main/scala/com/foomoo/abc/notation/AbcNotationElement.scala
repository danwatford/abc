package com.foomoo.abc.notation

/**
  * Represents a line of ABC notation found in the header of a tune.
  */
sealed trait AbcNotationHeaderLine

case class AbcNotationHeaderInformationField(key: String, value: String) extends AbcNotationHeaderLine

case class AbcNotationHeaderLineComment(comment: String) extends AbcNotationHeaderLine

case class AbcNotationHeader(lines: List[AbcNotationHeaderLine])


/**
  * Represents an element of ABC notation found in the body of a tune.
  */
sealed trait AbcNotationBodyElement

case class AbcNotationBodyWhitespace(whitespace: String) extends AbcNotationBodyElement

case class AbcNotationBodyNewLine() extends AbcNotationBodyElement

case class AbcNotationBodyScoreLineBreak() extends AbcNotationBodyElement

case class AbcNotationBodyLineContinuation() extends AbcNotationBodyElement

case class AbcNotationNote(note: String) extends AbcNotationBodyElement

case class AbcNotationBrokenRythm(brokenRythmDirection: String) extends AbcNotationBodyElement

case class AbcNotationTriplet() extends AbcNotationBodyElement

case class AbcNotationTie() extends AbcNotationBodyElement

case class AbcNotationSlurStart() extends AbcNotationBodyElement

case class AbcNotationSlurEnd() extends AbcNotationBodyElement

case class AbcNotationRest(rest: String) extends AbcNotationBodyElement

case class AbcNotationChord(chord: String) extends AbcNotationBodyElement

case class AbcNotationUnisonStart() extends AbcNotationBodyElement

case class AbcNotationUnisonEnd() extends AbcNotationBodyElement

case class AbcNotationGraceStart() extends AbcNotationBodyElement

case class AbcNotationGraceEnd() extends AbcNotationBodyElement

case class AbcNotationBar(barMarker: String) extends AbcNotationBodyElement

case class AbcNotationRepeat(repeatMarker: String) extends AbcNotationBodyElement

case class AbcNotationNumberedRepeat(number: Int) extends AbcNotationBodyElement

case class AbcNotationBodyComment(comment: String) extends AbcNotationBodyElement

case class AbcNotationBodyInformationField(key: String, value: String) extends AbcNotationBodyElement

/**
  * Represents the body of an ABC notation tune.
  *
  * @param elements The lines making up the body of an ABC tune.
  */
case class AbcNotationBody(elements: List[AbcNotationBodyElement])

/**
  * Represents an ABC notation tune.
  *
  * @param header Tune header.
  * @param body   Tune body.
  */
case class AbcNotationTune(header: AbcNotationHeader, body: AbcNotationBody)

/**
  * Represents the header of an ABC file.
 *
  * @param lines The header lines from the file header.
  */
case class AbcNotationFileHeader(lines: List[AbcNotationHeaderLine])

/**
  * Represents an ABC notation file.
  *
  * @param version The version field from the ABC notation file.
  * @param tunes   The tunes from the ABC notation file.
  */
case class AbcNotationFile(version: String, fileHeader: AbcNotationFileHeader, tunes: List[AbcNotationTune])

