package com.foomoo.abc

/**
  * Represents an element of ABC notation. Does not reflect the structure of a tune.
  */
sealed trait AbcNotationElement

/**
  * Represents a line of ABC notation found in the header of a tune.
  */
sealed trait AbcNotationHeaderLine

case class AbcNotationHeaderInformationField(key: Char, value: String) extends AbcNotationHeaderLine

case class AbcNotationHeaderLineComment(comment: String) extends AbcNotationHeaderLine

case class AbcNotationHeader(lines: List[AbcNotationHeaderLine])


/**
  * Represents an element of ABC notation found in the body of a tune.
  */
sealed trait AbcNotationBodyElement

case class AbcWhitespaceNotation(whitespace: String) extends AbcNotationBodyElement

case class AbcBodyLineContinuation() extends AbcNotationBodyElement

case class AbcNoteNotation(note: String) extends AbcNotationBodyElement

case class AbcBrokenRythmNotation(direction: String) extends AbcNotationBodyElement

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

case class AbcBarNotation(marker: String) extends AbcNotationBodyElement

case class AbcRepeatNotation(marker: String) extends AbcNotationBodyElement

case class AbcNumberedRepeatNotation(marker: String) extends AbcNotationBodyElement

case class AbcBodyInlineComment(comment: String) extends AbcNotationBodyElement

case class AbcBodyInlineInformationField(key: Char, value: String) extends AbcNotationBodyElement


/**
  * Represents a line of ABC notation found in the body of a tune.
  */
sealed trait AbcNotationBodyLine

case class AbcNotationBodyLineOfElements(elements: List[AbcNotationBodyElement]) extends AbcNotationBodyLine

case class AbcNotationBodyInformationField(key: Char, value: String) extends AbcNotationBodyLine

case class AbcNotationBodyLineComment(comment: String) extends AbcNotationBodyLine

/**
  * Represents the body of an ABC notation tune.
  *
  * @param lines The lines making up the body of an ABC tune.
  */
case class AbcNotationBody(lines: List[AbcNotationBodyLine])

/**
  * Represents an ABC notation tune.
  *
  * @param header Tune header.
  * @param body   Tune body.
  */
case class AbcTuneNotation(header: AbcNotationHeader, body: AbcNotationBody)
