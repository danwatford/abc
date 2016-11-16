package com.foomoo.abc.notation.processing

import com.foomoo.abc.notation._

/**
  * Functions to convert an Abc Notation objects to strings representing the original ABC notation text.
  */
object AbcNotationStringBuilder {


  def headerToString(notationHeader: AbcNotationHeader): String = notationHeader.lines.map {
    case AbcNotationHeaderLineComment(comment) => s"% $comment"
    case AbcNotationHeaderInformationField(key, value) => s"$key:$value"
  }.mkString("\n")

  def bodyToString(notationBody: AbcNotationBody): String = notationBody.elements.map {
    case AbcNotationBodyWhitespace(whitespace) => " "
    case AbcNotationBodyNewLine() => "\n"
    case AbcNotationBodyScoreLineBreak() => "$"
    case AbcNotationBodyLineContinuation() => "\\"
    case AbcNotationNote(note, noteLength) => note + noteLength
    case AbcNotationBrokenRhythm(brokenRhythmDirection) => brokenRhythmDirection
    case AbcNotationTriplet() => "(3"
    case AbcNotationTie() => "-"
    case AbcNotationSlurStart() => "("
    case AbcNotationSlurEnd() => ")"
    case AbcNotationRest(rest) => rest
    case AbcNotationChord(chord) => "\"" + chord + "\""
    case AbcNotationUnisonStart() => "["
    case AbcNotationUnisonEnd() => "]"
    case AbcNotationGraceStart() => "{"
    case AbcNotationGraceEnd() => "}"
    case AbcNotationBar(barMarker) => barMarker
    case AbcNotationRepeat(repeatMarker) => repeatMarker
    case AbcNotationNumberedRepeat(number) => s"|[$number"
    case AbcNotationBodyComment(comment) => s"% $comment\n"
    case AbcNotationBodyInformationField(key, value) => s"$key:$value\n"
    case AbcNotationBodyInlineInformationField(key, value) => s"[${key}:${value}]"
  }.mkString

  def tuneToString(notationTune: AbcNotationTune): String =
    headerToString(notationTune.header) + "\n" + bodyToString(notationTune.body)
}
