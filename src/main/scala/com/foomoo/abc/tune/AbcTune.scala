package com.foomoo.abc.tune

import scala.collection.mutable.ListBuffer

class AbcTune(builder: AbcTuneBuilder) {
  val reference: Option[String] = builder.reference
  val title: Option[String] = builder.title
  val meter: Option[String] = builder.meter
  val key: String = builder.key
  val composer: Option[String] = builder.composer
  val bodyElements: Seq[AbcStructuralElement] = builder.bodyElements.toList

  override def toString: String = String.format("AbcTune(%s, %s, %s, %s, %s, %s)", reference, title, meter, key, composer, bodyElements)

}

sealed trait AbcStructuralElement

case class AbcRepeat(xs: Seq[AbcBar]) extends AbcStructuralElement

case class AbcBar(xs: Seq[AbcNoteElement]) extends AbcStructuralElement


sealed trait AbcNoteElement

case class AbcNote(note: String) extends AbcNoteElement

case class AbcBrokenRythm(note1: AbcNote, note2: AbcNote) extends AbcNoteElement

case class AbcTriplet(note1: AbcNote, note2: AbcNote, note3: AbcNote) extends AbcNoteElement

case class AbcChord(chord: String) extends AbcNoteElement

class AbcTuneBuilder {

  var reference: Option[String] = None
  var title: Option[String] = None
  var meter: Option[String] = None
  var key: String = null
  var composer: Option[String] = None
  var bodyElements = new ListBuffer[AbcStructuralElement]

  def setReference(reference: String): AbcTuneBuilder = {
    this.reference = Some(reference)
    this
  }

  def setTitle(title: String): AbcTuneBuilder = {
    this.title = Some(title)
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

  def build(): AbcTune = {
    if (null == key) {
      throw new IllegalArgumentException("Cannot build AbcTune with null key")
    }

    new AbcTune(this)
  }

}