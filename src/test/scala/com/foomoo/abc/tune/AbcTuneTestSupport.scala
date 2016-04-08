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


}
