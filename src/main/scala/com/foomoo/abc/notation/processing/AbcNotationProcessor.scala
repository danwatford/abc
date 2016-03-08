package com.foomoo.abc.notation.processing

import com.foomoo.abc.notation._

class AbcNotationProcessor {

}

/**
  * Provides methods to process an AbcTuneNotation.
  */
object AbcNotationProcessor {

  /**
    * Processes the given AbcTuneNotation to produce a new AbcTuneNotation with comments removed.
    *
    * @param tuneNotation The tune notation to process.
    * @return The comment stripped tune notation.
    */
  def stripComments(tuneNotation: AbcTuneNotation): AbcTuneNotation = {

    val header: AbcNotationHeader = tuneNotation.header
    val body: AbcNotationBody = tuneNotation.body

    val commentStrippedHeaderLines = header.lines filter {
      case headerLine: AbcNotationHeaderLineComment => false
      case _ => true
    }
    val commentStrippedHeader = AbcNotationHeader(commentStrippedHeaderLines)

    val commentStrippedBodyElements = body.elements filter {
      case commentElement: AbcBodyCommentNotation => false
      case _ => true
    }
    val commentStrippedBody = AbcNotationBody(commentStrippedBodyElements)

    AbcTuneNotation(commentStrippedHeader, commentStrippedBody)
  }

  /**
    * Processes the given AbcTuneNotation to produce a new AbcTuneNotation with comments stripped
    * and lines joined in place of line continuation notation elements.
    *
    * @param tuneNotation The tune notation to process.
    * @return The normalised tune notation.
    */
  def normalise(tuneNotation: AbcTuneNotation): AbcTuneNotation = {
    // TODO
    stripComments(tuneNotation)
  }

}
