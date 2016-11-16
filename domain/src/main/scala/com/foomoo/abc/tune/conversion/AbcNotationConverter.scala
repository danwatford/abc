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
    tuneBuilder.setAbcNotation(tuneNotation)

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
    var inRepeat = false

    var numberedRepeatSection = 0

    var unisonNoteSequence: List[AbcNote] = List()
    var noteElementSequence: List[AbcNoteElement] = List()
    var barSequence: List[AbcBar] = List()
    var numberedRepeatSequencesMap: Map[Int, List[AbcBar]] = Map()
    var bodySequence: List[AbcStructuralElement] = List()

    elements.foreach {
      case AbcNotationNote(note, noteLength) =>
        if (inGraceNoteSequence) {
          () // Drop grace notes
        } else if (inUnisonSequence) {
          unisonNoteSequence = unisonNoteSequence :+ AbcNote(note)
        } else {
          noteElementSequence = noteElementSequence :+ AbcNote(note)
        }

      // Ignore
      case AbcNotationBrokenRhythm(_) => ()

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

      case AbcNotationRepeat(repeatMarker) =>

        def repeatStart(): Unit = {
          inRepeat = true
          if (noteElementSequence.nonEmpty) {
            val bar = AbcBar(noteElementSequence)
            noteElementSequence = List()
            barSequence = barSequence :+ bar
          }

          // Any bars in the bar sequence will not be part of a repeat, therefore add to body sequence.
          bodySequence = barSequence ++ bodySequence
          barSequence = List()
        }

        def repeatEnd(): Unit = {
          inRepeat = false
          if (noteElementSequence.nonEmpty) {
            val bar = AbcBar(noteElementSequence)
            noteElementSequence = List()
            barSequence = barSequence :+ bar
          }

          val nextStructuralElement: AbcStructuralElement = if (numberedRepeatSection > 0) {
            numberedRepeatSequencesMap = numberedRepeatSequencesMap + ((numberedRepeatSection, barSequence))
            numberedRepeatSection = 0

            AbcNumberedRepeat(numberedRepeatSequencesMap(0), numberedRepeatSequencesMap - 0)
          } else {
            // Repeat any bars in the bar sequence.
            AbcRepeat(barSequence)
          }

          bodySequence = bodySequence :+ nextStructuralElement
          barSequence = List()
        }

        repeatMarker match {
          case "|:" => repeatStart()
          case ":|" | ":|]" => repeatEnd()
          case ":||:" =>
            repeatEnd()
            repeatStart()
        }

      case AbcNotationNumberedRepeat(repeatNumber) =>
        if (noteElementSequence.nonEmpty) {
          val bar = AbcBar(noteElementSequence)
          noteElementSequence = List()
          barSequence = barSequence :+ bar
        }

        if (numberedRepeatSection == 0) {
          // We are entering the numbered repeat phase, take all read bars and notes not in a repeat block
          // and capture into the common part (part-0) of the numbered repeat block.
          numberedRepeatSequencesMap = Map(0 -> barSequence)
        } else {
          numberedRepeatSequencesMap = numberedRepeatSequencesMap + (numberedRepeatSection -> barSequence)
        }
        barSequence = List()

        // Start capturing numbered repeat blocks.
        numberedRepeatSection = repeatNumber

      case AbcNotationBodyInformationField("K", keyChange) =>
        bodySequence = bodySequence :+ AbcKeyChange(keyChange)

      case AbcNotationBodyInformationField(_, _) => ()
      case AbcNotationBodyInlineInformationField(_, _) => ()
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

    if (inRepeat) {
      bodySequence :+ AbcRepeat(barSequence)
    } else {
      bodySequence ++ barSequence
    }
  }

}
