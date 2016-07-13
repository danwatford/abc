package com.foomoo.abc.service

import com.foomoo.abc.tune.{AbcNote, AbcTune}

import scala.collection.mutable

/**
  * Service to find tunes containing matching subs-sequences of notes.
  */
object SubsequenceMatchService {

  type NoteSequence = Seq[AbcNote]

  /** For the given collection of tunes, find all unique note sub-sequences of the given length and then
    * return a map of those sub-sequences to the tunes they appear in.
    *
    * No analysis of the tune structure is performed, instead the sequence of notes in a tune are found by filtering
    * out all but the notes in the tune's body.
    *
    * @param sequenceLength The length of the sub-sequences to find.
    * @param tunes          The collection of tunes to examine.
    * @return A map of note sub-sequences to the collection of tunes that contain them.
    */
  def getSubSequenceTunes(sequenceLength: Int, tunes: Seq[AbcTune]): Map[NoteSequence, Set[AbcTune]] = {

    val configuredSequenceToSubSequences = tuneSequenceToSubSequences(sequenceLength) _

    // Map each tune to a sequence of its notes.
    // Limit the number of tunes to process with take() function.
    val tuneToNoteSequenceMap: Map[AbcTune, NoteSequence] = tunes.map(tune => (tune, tune.getNotes)).toMap

    // For each tune sequence, build a set of note sub-sequences that appear.
    val tuneToSubSequencesMap: Map[AbcTune, Set[NoteSequence]] = tuneToNoteSequenceMap.mapValues(configuredSequenceToSubSequences)

    // Get all unique sub-sequences across all tunes
    val allSubSequences: Set[NoteSequence] = tuneToSubSequencesMap.flatMap(_._2).toSet

    // Build a map of sub-sequences to a list of tunes that they contain.
    val subSequenceTunesMap = new mutable.HashMap[NoteSequence, mutable.Set[AbcTune]] with mutable.MultiMap[NoteSequence, AbcTune]
    tuneToSubSequencesMap.foreach { entry: (AbcTune, Set[NoteSequence]) =>
      val (tune, subSequences) = entry
      subSequences.foreach(ss => subSequenceTunesMap.addBinding(ss, tune))
    }

    // The values in the multimap are a mutable set, convert values to immutable set then convert mutable
    // map to immutable map.
    subSequenceTunesMap.mapValues(_.toSet).toMap
  }

  def tuneSequenceToSubSequences(sequenceLength: Int)(tuneSequence: NoteSequence): Set[NoteSequence] = {
      tuneSequence.sliding(sequenceLength).toSet
  }

}
