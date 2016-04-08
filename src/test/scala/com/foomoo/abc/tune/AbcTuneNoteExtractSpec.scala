package com.foomoo.abc.tune

import com.foomoo.abc.UnitSpec
import com.foomoo.abc.tune.AbcTuneTestSupport._

/**
  * Tests regarding the extraction of note sequences from an AbcTune
  */
class AbcTuneNoteExtractSpec extends UnitSpec {

  "AbcTune" should "extract notes for a single bar" in {
    val tune = tuneWithHeader(BAR_A :: Nil)

    tune.getNotes should contain (NOTE_A)
  }

  it should "extract notes for multiple bars" in {
    val tune = tuneWithHeader(BAR_A :: BAR_CD :: Nil)

    tune.getNotes should be (List(NOTE_A, NOTE_C, NOTE_D))
  }

  it should "extract notes for repeat sections" in {
    val tune = tuneWithHeader(List(AbcRepeat(List(BAR_A, BAR_CD))))

    tune.getNotes should be (List(NOTE_A, NOTE_C, NOTE_D, NOTE_A, NOTE_C, NOTE_D))
  }

  it should "extract notes for numbered repeat sections" in {
    val tune = tuneWithHeader(List(AbcNumberedRepeat(List(BAR_AB),
      Map(1 -> List(BAR_CD), 2 -> List(BAR_EF)))))

    tune.getNotes should be (List(NOTE_A, NOTE_B, NOTE_C, NOTE_D, NOTE_E, NOTE_F))
  }

  def tuneWithHeader(tuneBody: Seq[AbcStructuralElement]): AbcTune =
    new AbcTuneBuilder().setReference("").setKey("C").setBodyElements(tuneBody).build()

}
