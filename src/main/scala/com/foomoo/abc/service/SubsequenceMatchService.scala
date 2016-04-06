package com.foomoo.abc.service

import java.util.Calendar

import com.foomoo.abc.notation.processing.AbcNotationProcessor
import com.foomoo.abc.notation.{AbcNotationNote, AbcNotationTune}

import scala.collection.mutable

/**
  * Service to find tunes containing matching subs-equences of notes.
  */
object SubsequenceMatchService {

  type NoteSequence = Seq[AbcNotationNote]

  /** For the given collection of tunes, find all unique note sub-sequences of the given length and then return
    * a map of those sub-sequences to the tunes they appear in.
    *
    * No analysis of the tune structure is performed, instead the sequence of notes in a tune are found by filtering
    * out all but the notes in the tune's body.
    *
    * @param sequenceLength The length of the sub-sequences to find.
    * @param tunes          The collection of tunes to examine.
    * @return A map of note sub-sequences to the collection of tunes that contain them.
    */
  def getSubsequenceTunes(sequenceLength: Int, tunes: Seq[AbcNotationTune]): Map[NoteSequence, Set[AbcNotationTune]] = {

    val configuredSequenceToSubsequences = tuneSequenceToSubsequences(sequenceLength) _

    // Map each tune to a sequence of its notes.
    // Limit the number of tunes to process with take() function.
    val tuneToNoteSequenceMap: Map[AbcNotationTune, NoteSequence] = tunes.map(tune => (tune, AbcNotationProcessor.simpleNoteExtract(tune))).toMap

    // For each tune sequence, build a set of note sub-sequences that appear.
    val tuneToSubsequencesMap: Map[AbcNotationTune, Set[NoteSequence]] = tuneToNoteSequenceMap.mapValues(configuredSequenceToSubsequences)

    // Get all unique sub-sequences across all tunes
    val allSubsequences: Set[NoteSequence] = tuneToSubsequencesMap.flatMap(_._2).toSet

    println(Calendar.getInstance().getTime)
    println(s"Have ${allSubsequences.size} sub-sequences")

    // Build a map of sub-sequences to a list of tunes that they contain.
    val subsequenceTunesMap = new mutable.HashMap[NoteSequence, mutable.Set[AbcNotationTune]] with mutable.MultiMap[NoteSequence, AbcNotationTune]
    tuneToSubsequencesMap.foreach { entry: (AbcNotationTune, Set[NoteSequence]) =>
      val (tune, subsequences) = entry
      subsequences.foreach(ss => subsequenceTunesMap.addBinding(ss, tune))
    }

    println(Calendar.getInstance().getTime)
    println(s"Have ${subsequenceTunesMap.size} sub-sequences in subsequencesTunesMap")

    // The values in the mutimap are a mutable set, convert values to immutable set then convert mutable
    // map to immutable map.
    subsequenceTunesMap.mapValues(_.toSet).toMap
  }

  def tuneSequenceToSubsequences(sequenceLength: Int)(tuneSequence: Seq[AbcNotationNote]): Set[Seq[AbcNotationNote]] = {
    tuneSequence.sliding(sequenceLength).toSet
  }

}
