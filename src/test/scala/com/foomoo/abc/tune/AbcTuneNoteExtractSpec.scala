package com.foomoo.abc.tune

import com.foomoo.abc.UnitSpec
import com.foomoo.abc.tune.AbcTuneTestSupport._

/**
  * Tests regarding the extraction of note sequences from an AbcTune
  */
class AbcTuneNoteExtractSpec extends UnitSpec {

  "AbcTune" should "extract notes for a single bar" in {
    val tune = tuneWithHeader(AbcBar(NOTE_A :: Nil) :: Nil)

    tune.getNotes should contain (NOTE_A)
  }

  it should "extract notes for multiple bars" in {
    val tune = tuneWithHeader(AbcBar(NOTE_A :: Nil) :: AbcBar(NOTE_B :: NOTE_C :: Nil) :: Nil)

    tune.getNotes should be (List(NOTE_A, NOTE_B, NOTE_C))
  }

  it should "extract notes for repeat sections" in {
    val tune = tuneWithHeader(List(AbcRepeat(List(AbcBar(NOTE_A :: Nil), AbcBar(NOTE_B :: NOTE_C :: Nil)))))

    tune.getNotes should be (List(NOTE_A, NOTE_B, NOTE_C, NOTE_A, NOTE_B, NOTE_C))
  }

  def tuneWithHeader(tuneBody: Seq[AbcStructuralElement]): AbcTune =
    new AbcTuneBuilder().setReference("").setKey("C").setBodyElements(tuneBody).build()

}
