package com.foomoo.abc.tune.conversion

import com.foomoo.abc.notation._
import com.foomoo.abc.tune._

/**
  * Converts an AbcNotationTune object to an AbcTune object, determining the structure (bars, repeats, etc.) of the tune.
  */
object AbcNotationConverter {

  /**
    * Converts the given AbcNotationTune into an AbcTune
    *
    * @param tuneNotation The ABC tune notation to convert.
    * @return The created AbcTune.
    */
  def convertTune(tuneNotation: AbcNotationTune): AbcTune = {
    convertTune(tuneNotation, AbcNotationFileHeader(Nil))
  }

  /**
    * Converts the given AbcNotationTne into an AbcTune, apply any information fields from the given AbcNotationFileHeader.
    *
    * @param tuneNotation       The ABC tune notation to convert.
    * @param fileHeaderNotation The ABC file header to use during the conversion.
    * @return The created AbcTune.
    */
  def convertTune(tuneNotation: AbcNotationTune, fileHeaderNotation: AbcNotationFileHeader): AbcTune = {

    val tuneBuilder = new AbcTuneBuilder()

    fileHeaderNotation.lines ++ tuneNotation.header.lines foreach {
      case AbcNotationHeaderInformationField("X", refString) => tuneBuilder.setReference(refString)
      case AbcNotationHeaderInformationField("T", titleString) => tuneBuilder.addTitle(titleString)
      case AbcNotationHeaderInformationField("M", meterString) => tuneBuilder.setMeter(meterString)
      case AbcNotationHeaderInformationField("C", composerString) => tuneBuilder.setComposer(composerString)
      case AbcNotationHeaderInformationField("K", keyString) => tuneBuilder.setKey(keyString)
      case _ => ()
    }

    tuneBuilder.setBodyElements(convertBody(tuneNotation.body))

    tuneBuilder.build()
  }

  def convertBody(notationBody: AbcNotationBody): List[AbcStructuralElement] = {

    val elements: List[AbcNotationBodyElement] = notationBody.elements

    var inUnisonSequence = false
    var inGraceNoteSequence = false

    var unisonNoteSequence: List[AbcNote] = List()
    var noteElementSequence: List[AbcNoteElement] = List()
    var barSequence: List[AbcBar] = List()
    var bodySequence: List[AbcStructuralElement] = List()

    elements.foreach {
      case AbcNotationNote(note) =>
        if (inGraceNoteSequence) {
          () // Drop grace notes
        } else if (inUnisonSequence) {
          unisonNoteSequence = unisonNoteSequence :+ AbcNote(note)
        } else {
          noteElementSequence = noteElementSequence :+ AbcNote(note)
        }

      // Ignore
      case AbcNotationBrokenRythm(_) => ()

      // Ignore
      case AbcNotationTriplet() => ()

      case AbcNotationChord(chord) => noteElementSequence = noteElementSequence :+ AbcChord(chord)

      case AbcNotationUnisonStart() =>
        inUnisonSequence = true
        unisonNoteSequence = List()

      case AbcNotationUnisonEnd() =>
        inUnisonSequence = false
        noteElementSequence = noteElementSequence :+ AbcUnison(unisonNoteSequence)

      case AbcNotationGraceStart() =>
        inGraceNoteSequence = true

      case AbcNotationGraceEnd() =>
        inGraceNoteSequence = false

      // Ignore
      case AbcNotationSlurStart() => ()

      // Ignore
      case AbcNotationSlurEnd() => ()

      // Ignore
      case AbcNotationTie() => ()

      case AbcNotationBar(barMarker) =>
        val bar = AbcBar(noteElementSequence)
        noteElementSequence = List()
        barSequence = barSequence :+ bar

      case AbcNotationRepeat(repeatMarker) => repeatMarker match {
        case "|:" => {
          if (noteElementSequence.nonEmpty) {
            val bar = AbcBar(noteElementSequence)
            noteElementSequence = List()
            barSequence = barSequence :+ bar
          }

          // Any bars in the bar sequence will not be part of a repeat, therefore add to body sequence.
          bodySequence = barSequence ++ bodySequence
          barSequence = List()
        }
        case ":|" | ":|]" => {
          if (noteElementSequence.nonEmpty) {
            val bar = AbcBar(noteElementSequence)
            noteElementSequence = List()
            barSequence = barSequence :+ bar
          }

          // Repeat any bars in the bar sequence.
          bodySequence = bodySequence :+ AbcRepeat(barSequence)
          barSequence = List()
        }
      }

      case AbcNotationNumberedRepeat(marker) => {
        // TODO
      }

      case AbcNotationBodyInformationField("K", keyChange) =>
        bodySequence = bodySequence :+ AbcKeyChange(keyChange)

      case AbcNotationBodyInformationField(_, _) => ()
      case AbcNotationBodyWhitespace(_) => ()
      case AbcNotationBodyNewLine() => ()
      case AbcNotationBodyLineContinuation() => ()
      case AbcNotationBodyScoreLineBreak() => ()
      case AbcNotationBodyComment(_) => ()
      case AbcNotationRest(_) => ()
    }

    if (noteElementSequence.nonEmpty) {
      val bar = AbcBar(noteElementSequence)
      noteElementSequence = List()
      barSequence = barSequence :+ bar
    }

    bodySequence = bodySequence ++ barSequence

    bodySequence
  }

}
