package com.foomoo.abc

class AbcTune(val reference: Option[String], val title: Option[String], val meter: Option[String], val key: String, val composer: Option[String]) {

}

class AbcTuneBuilder {

  private var reference: Option[String] = None
  private var title: Option[String] = None
  private var meter: Option[String] = None
  private var key: String = null
  private var composer: Option[String] = None

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

  def build(): AbcTune = {
    if (null == key) {
      throw new IllegalArgumentException("Cannot build AbcTune with null key")
    }

    new AbcTune(reference, title, meter, key, composer)
  }

}