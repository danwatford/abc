package com.foomoo.abc.notation

/**
  * Represents an element of ABC notation. Does not reflect the structure of a tune.
  */
sealed trait AbcNotationElement

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

case class AbcBodyWhitespaceNotation(whitespace: String) extends AbcNotationBodyElement

case class AbcBodyNewLine() extends AbcNotationBodyElement

case class AbcBodyScoreLineBreak() extends AbcNotationBodyElement

case class AbcBodyLineContinuation() extends AbcNotationBodyElement

case class AbcNoteNotation(note: String) extends AbcNotationBodyElement

case class AbcBrokenRythmNotation(brokenRythmDirection: String) extends AbcNotationBodyElement

case class AbcTripletNotation() extends AbcNotationBodyElement

case class AbcTieNotation() extends AbcNotationBodyElement

case class AbcSlurStartNotation() extends AbcNotationBodyElement

case class AbcSlurEndNotation() extends AbcNotationBodyElement

case class AbcRestNotation(rest: String) extends AbcNotationBodyElement

case class AbcChordNotation(chord: String) extends AbcNotationBodyElement

case class AbcUnisonStartNotation() extends AbcNotationBodyElement

case class AbcUnisonEndNotation() extends AbcNotationBodyElement

case class AbcGraceStartNotation() extends AbcNotationBodyElement

case class AbcGraceEndNotation() extends AbcNotationBodyElement

case class AbcBarNotation(barMarker: String) extends AbcNotationBodyElement

case class AbcRepeatNotation(repeatMarker: String) extends AbcNotationBodyElement

case class AbcNumberedRepeatNotation(numberedRepeatMarker: String) extends AbcNotationBodyElement

case class AbcBodyCommentNotation(comment: String) extends AbcNotationBodyElement

case class AbcBodyInformationFieldNotation(key: String, value: String) extends AbcNotationBodyElement

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
case class AbcTuneNotation(header: AbcNotationHeader, body: AbcNotationBody)
