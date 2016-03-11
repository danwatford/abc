package com.foomoo.abc.notation.processing

import com.foomoo.abc.notation._

/**
  * Provides methods to process an AbcTuneNotation.
  */
object AbcNotationProcessor {

  /**
    * Processes the list of head lines to remove any comments.
    *
    * @param headerLines The header notation lines to process.
    * @return The header notation lines with comment lines removed.
    */
  def removeHeaderComments(headerLines: List[AbcNotationHeaderLine]) = headerLines filter {
    case _: AbcNotationHeaderLineComment => false
    case _ => true
  }

  /**
    * Processes the list of body notation elements to remove any comments.
    *
    * @param bodyElements The body notation elements to process.
    * @return The body notation elements with comments removed.
    */
  def removeBodyComments(bodyElements: List[AbcNotationBodyElement]) = bodyElements filter {
    case commentElement: AbcBodyCommentNotation => false
    case _ => true
  }

  /**
    * Processes the list of header lines, joining any continuation information fields with the previous
    * information field.
    *
    * The caller should ensure the given AbcNotationHeader is free of AbcNotationHeaderLineComments as these
    * could interfere with joining of lines if positioned between an information field and its continuation
    * information field.
    *
    * @param headerLines The header notation lines to process.
    * @return The line-joined header notation lines.
    */
  def joinHeaderLines(headerLines: List[AbcNotationHeaderLine]): List[AbcNotationHeaderLine] = {

    headerLines.foldRight[List[AbcNotationHeaderLine]](List()) { (element, accumulator) =>

      // We only join information fields. If the element is not an information field it cannot be merged
      // with any continuation information fields. Such elements are prepended to the accumulator.
      element match {
        case AbcNotationHeaderInformationField(key, value) =>
          accumulator match {
            case Nil => List(element)

            case AbcNotationHeaderInformationField('+', continuationFieldValue) :: xs =>
              // Merge the continuation field at the head of the accumulator with the new element and prepend
              // to the tail of the accumulator.
              AbcNotationHeaderInformationField(key, value + continuationFieldValue) :: xs

            case _ => element :: accumulator
          }

        case _ => element :: accumulator
      }
    }
  }

  /**
    * Processes the list of body elements, finding and removing any sequences consisting of a
    * line continuation element followed by an optional line break.
    *
    * @param bodyElements The tune body elements to process.
    * @return The line-joined tune body elements.
    */
  def joinBodyLine(bodyElements: List[AbcNotationBodyElement]): List[AbcNotationBodyElement] = {

    bodyElements match {
      case Nil => Nil
      case _ =>

        // Look at adjacent pairs of elements, taking the second element in the pair unless the pair
        // is a line continuation with a line break, in which case map the pair to a line continuation.
        // To ensure the first element is included in the result an extra copy is inserted at the front
        // of the element list.
        val elements: List[AbcNotationBodyElement] = bodyElements.head :: bodyElements.head :: bodyElements.tail
        val elementPairs: List[(AbcNotationBodyElement, AbcNotationBodyElement)] = elements.zipAll(elements.tail, AbcBodyNewLine(), AbcBodyNewLine())

        val mappedElements: List[AbcNotationBodyElement] = elementPairs.map {
          case x if x._1.isInstanceOf[AbcBodyLineContinuation] && x._2.isInstanceOf[AbcBodyNewLine] => AbcBodyLineContinuation()
          case x if x._2.isInstanceOf[AbcBodyLineContinuation] => AbcBodyLineContinuation()
          case x => x._2
        }

        // Filter out all line continuations.
        val filteredElements: List[AbcNotationBodyElement] = mappedElements.filter {
          case continuation: AbcBodyLineContinuation => false
          case _ => true
        }

        filteredElements
    }
  }

  /**
    * Processes the given AbcTuneNotation to produce a new AbcTuneNotation with comments stripped
    * and lines joined in place of line continuation notation elements.
    *
    * @param tuneNotation The tune notation to process.
    * @return The normalised tune notation.
    */
  def normalise(tuneNotation: AbcTuneNotation): AbcTuneNotation = {

    val joinedHeaderLines: List[AbcNotationHeaderLine] = joinHeaderLines(removeHeaderComments(tuneNotation.header.lines))
    val joinedBodyElements: List[AbcNotationBodyElement] = joinBodyLine(removeBodyComments(tuneNotation.body.elements))

    AbcTuneNotation(AbcNotationHeader(joinedHeaderLines), AbcNotationBody(joinedBodyElements))
  }

}
