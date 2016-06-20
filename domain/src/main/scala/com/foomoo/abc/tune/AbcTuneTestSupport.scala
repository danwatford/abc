package com.foomoo.abc.tune

/**
  * Contains common items to support testing.
  */
object AbcTuneTestSupport {

  val NOTE_A = AbcNote("A")
  val NOTE_B = AbcNote("B")
  val NOTE_C = AbcNote("C")
  val NOTE_D = AbcNote("D")
  val NOTE_E = AbcNote("E")
  val NOTE_F = AbcNote("F")

  val BAR_A = AbcBar(List(NOTE_A))
  val BAR_B = AbcBar(List(NOTE_B))
  val BAR_C = AbcBar(List(NOTE_C))
  val BAR_D = AbcBar(List(NOTE_D))

  val BAR_AB = AbcBar(List(NOTE_A, NOTE_B))
  val BAR_CD = AbcBar(List(NOTE_C, NOTE_D))
  val BAR_EF = AbcBar(List(NOTE_E, NOTE_F))

  val REP1 = AbcRepeat(List(BAR_AB, BAR_CD))
  val REP2 = AbcRepeat(List(BAR_EF))

  val NUMBERED_REP1 = AbcNumberedRepeat(List(BAR_AB, BAR_CD), Map(1 -> Seq(BAR_EF), 2 -> Seq(BAR_A)))

  def getTestTune: AbcTune = {

    val builder = new AbcTuneBuilder

    builder.addTitle("Title 1")
    builder.addTitle("Title 2")
    builder.addTitle("Title 3")

    builder.setKey("C")

    builder.setMeter("4/4")

    builder.setComposer("Composer 1")

    builder.setReference("1")

    builder.addNoteElement(BAR_AB).addNoteElement(BAR_CD)
    builder.addNoteElement(REP1).addNoteElement(REP2)
    builder.addNoteElement(NUMBERED_REP1)

    builder.build()
  }

}
