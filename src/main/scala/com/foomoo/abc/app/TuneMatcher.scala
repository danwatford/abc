package com.foomoo.abc.app

import java.nio.file.{Path, Paths}
import java.util.Calendar

import com.foomoo.abc.notation.parsing.AbcNotationParser
import com.foomoo.abc.notation.processing.AbcNotationProcessor
import com.foomoo.abc.notation.{AbcNotationHeader, AbcNotationHeaderInformationField, AbcNoteNotation, AbcTuneNotation}

import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

/**
  * An example application to parse a collection of ABC tunes and search for matching note sequences between them.
  */
object TuneMatcher extends App {

  // Arguments are paths to tune files.
  println("Args: " + args.toSeq)
  private val paths: Seq[Path] = args.map(Paths.get(_))
  println("Paths: " + paths)

  private val tuneFileStrings: Seq[String] = paths.map(p => Source.fromFile(p.toFile, "UTF-8").mkString)

  // Convert each string to tunes.
  println(s"Reading ${tuneFileStrings.length} tune files")
  val tunes: Seq[AbcTuneNotation] = tuneFileStrings.flatMap { tuneFileString =>
    val input = new CharSequenceReader(tuneFileString)
    AbcNotationParser.tunes(input) match {
      case AbcNotationParser.Success(tunes, _) => tunes
      case AbcNotationParser.NoSuccess(msg, next) => throw new IllegalArgumentException(
        msg + "\nNext is: " + next.pos)
    }
  }

  println(Calendar.getInstance().getTime)
  println(s"Read ${tunes.length} tunes")


  // Map each tune to a sequence of its notes.
  // Limit the number of tunes to process with take() function.
  private val tuneSequencesMap: Map[AbcTuneNotation, Seq[AbcNoteNotation]] = tunes take 100 map (tune => (tune, AbcNotationProcessor.simpleNoteExtract(tune))) toMap

  // For each tune sequence, build a set of 16 note sub-sequences that appear.
  private val tuneSubSequencesMap: Map[AbcTuneNotation, Set[Seq[AbcNoteNotation]]] = tuneSequencesMap.mapValues(tuneSequenceToSubSequences)

  // Get all unique sub-sequences
  private val allSubSequences: Set[Seq[AbcNoteNotation]] = tuneSubSequencesMap flatMap (_._2) toSet

  println(Calendar.getInstance().getTime)
  println(s"Have ${allSubSequences.size} sub-sequences")

  // For each sub-sequence, find the tunes which contain it.
  private val subSequenceTunesSeq: Set[(Seq[AbcNoteNotation], Seq[AbcTuneNotation])] = allSubSequences map { subSequence =>
    val subSequenceTunes = tuneSubSequencesMap
      .filter { case (tune, tuneSubSeqences) => tuneSubSeqences.contains(subSequence) }
      .map { case (tune, tuneSubSeqences) => tune }
      .toSeq

    (subSequence, subSequenceTunes)
  }
  println(Calendar.getInstance().getTime)

  // Filter out any sequences with only one matching tune.
  private val filteredSubSequencesTunesSeq: Set[(Seq[AbcNoteNotation], Seq[AbcTuneNotation])] = subSequenceTunesSeq.filter { case (subSequence, tuneSeq) => tuneSeq.length > 1 }

  println(Calendar.getInstance().getTime)
  println(s"Have ${filteredSubSequencesTunesSeq.size} filtered sub-sequences")

  filteredSubSequencesTunesSeq.foreach {case (subSequence, tuneSeq) =>

      val tuneTitles: Seq[String] = tuneSeq.map(tuneToTitle)

      val tuneTitlesFormatted = tuneTitles.mkString("--- ", "\n--- ", "")

      println(subSequence.map { case AbcNoteNotation(note) => note })
      println("appears in");
      println(tuneTitlesFormatted)
  }

  def tuneSequenceToSubSequences(tuneSequence: Seq[AbcNoteNotation]): Set[Seq[AbcNoteNotation]] = {
    tuneSequence.sliding(8).toSet
  }

  def tuneToTitle(tuneNotation: AbcTuneNotation): String = {
    headerValue(tuneNotation, "T").head
  }

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
