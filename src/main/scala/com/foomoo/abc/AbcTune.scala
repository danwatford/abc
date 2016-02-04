package com.foomoo.abc

import scala.collection.mutable.ListBuffer

class AbcTune(builder: AbcTuneBuilder) {
  val reference: Option[String] = builder.reference
  val title: Option[String] = builder.title
  val meter: Option[String] = builder.meter
  val key: String = builder.key
  val composer: Option[String] = builder.composer
  val noteElements: Seq[AbcNoteElement] = builder.noteElements.toList

  override def toString: String = String.format("AbcTune(%s, %s, %s, %s, %s, %s)", reference, title, meter, key, composer, noteElements)

}

sealed trait AbcNoteElement

case class AbcRepeat(xs: Seq[AbcNoteElement]) extends AbcNoteElement

case class AbcBar(xs: Seq[AbcNoteElement]) extends AbcNoteElement

case class AbcNote(note: String) extends AbcNoteElement

class AbcTuneBuilder {

  var reference: Option[String] = None
  var title: Option[String] = None
  var meter: Option[String] = None
  var key: String = null
  var composer: Option[String] = None
  var noteElements = new ListBuffer[AbcNoteElement]

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

  def addNoteElement(noteElement: AbcNoteElement): AbcTuneBuilder = {
    this.noteElements += noteElement
    this
  }

  def build(): AbcTune = {
    if (null == key) {
      throw new IllegalArgumentException("Cannot build AbcTune with null key")
    }

    new AbcTune(this)
  }

}