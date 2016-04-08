package com.foomoo.abc.tune

import scala.collection.mutable.ListBuffer

class AbcTune(builder: AbcTuneBuilder) {
  val reference: Option[String] = builder.reference
  val titles: List[String] = builder.titles
  val meter: Option[String] = builder.meter
  val key: String = builder.key
  val composer: Option[String] = builder.composer
  val bodyElements: List[AbcStructuralElement] = builder.bodyElements.toList

  override def toString: String = String.format("AbcTune(%s, %s, %s, %s, %s, %s)", reference, titles, meter, key, composer, bodyElements)

  /**
    * Gets a list of the notes present in the tune, repeated as needed to follow the structure described by
    * repeat sections.
    *
    * No conversion of notes according to the tune's key is carried out. This also means that changes of
    * key throughout the tune will not affect the notes produced by this method.
    *
    * @return A list of notes.
    */
  def getNotes: List[AbcNote] = {
    bodyElements.flatMap(structuralToNotesList)
  }

  private def structuralToNotesList(structural: AbcStructuralElement): List[AbcNote] = {
    val noteElements: Seq[AbcNoteElement] = structural match {
      case AbcBar(noteSequence) => noteSequence

      case AbcRepeat(barSequence) =>  barSequence.flatMap(structuralToNotesList) ++ barSequence.flatMap(structuralToNotesList)

      case AbcNumberedRepeat(commonBarSequence, numberSequencesMap) =>
        commonBarSequence.flatMap(structuralToNotesList) ++ numberSequencesMap.toSeq.sortBy(_._1).flatMap(_._2).flatMap(structuralToNotesList)

      case AbcKeyChange(_) => List()
    }

    noteElements.collect{ case note: AbcNote => note }.toList
  }
}

sealed trait AbcStructuralElement

case class AbcRepeat(xs: Seq[AbcBar]) extends AbcStructuralElement

case class AbcNumberedRepeat(xs: Seq[AbcBar], numberedSequences: Map[Int, Seq[AbcBar]]) extends AbcStructuralElement

case class AbcBar(xs: Seq[AbcNoteElement]) extends AbcStructuralElement

case class AbcKeyChange(key: String) extends AbcStructuralElement


sealed trait AbcNoteElement

case class AbcNote(note: String) extends AbcNoteElement

case class AbcBrokenRythm(note1: AbcNote, note2: AbcNote) extends AbcNoteElement

case class AbcTriplet(note1: AbcNote, note2: AbcNote, note3: AbcNote) extends AbcNoteElement

case class AbcChord(chord: String) extends AbcNoteElement

case class AbcUnison(notes: List[AbcNote]) extends AbcNoteElement

class AbcTuneBuilder {

  var reference: Option[String] = None
  var titles: List[String] = Nil
  var meter: Option[String] = None
  var key: String = null
  var composer: Option[String] = None
  var bodyElements = new ListBuffer[AbcStructuralElement]

  def setReference(reference: String): AbcTuneBuilder = {
    this.reference = Some(reference)
    this
  }

  def addTitle(title: String): AbcTuneBuilder = {
    this.titles = this.titles :+ title
    this
  }

  def setMeter(meter: String): AbcTuneBuilder = {
    this.meter = Some(meter)
    this
  }

  def setComposer(composer: String): AbcTuneBuilder = {
    this.composer = Some(composer)
    this
  }

  def setKey(key: String): AbcTuneBuilder = {
    this.key = key
    this
  }

  def addNoteElement(structuralElement: AbcStructuralElement): AbcTuneBuilder = {
    this.bodyElements += structuralElement
    this
  }

  def setBodyElements(bodyElements: Seq[AbcStructuralElement]): AbcTuneBuilder = {
    this.bodyElements.clear()
    this.bodyElements ++= bodyElements
    this
  }

  def build(): AbcTune = {
    if (null == key) {
      throw new IllegalArgumentException("Cannot build AbcTune with null key")
    }

    new AbcTune(this)
  }

}