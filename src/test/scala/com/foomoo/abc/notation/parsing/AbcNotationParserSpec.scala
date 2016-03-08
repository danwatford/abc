package com.foomoo.abc.notation.parsing

import java.net.URI

import com.foomoo.abc._
import com.foomoo.abc.notation._
import com.foomoo.abc.notation.AbcNotationTestSupport._

import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

class AbcNotationParserSpec extends UnitSpec {
  val resource: URI = getClass.getResource("/speed_the_plough.abc").toURI
  val multipleTuneResource: URI = getClass.getResource("/multiple_tunes.abc").toURI
  val pghSessionTunebookResource: URI = getClass.getResource("/pgh_session_tunebook.abc").toURI
  val tuneContents: String = Source.fromFile(resource, "UTF-8").mkString
  val multipleTuneContents: String = Source.fromFile(multipleTuneResource, "UTF-8").mkString
  val pghSessionTunebookContents: String = Source.fromFile(pghSessionTunebookResource, "UTF-8").mkString

  "The AbcNotationParser" should "read notes" in {
    assertResult(AbcNotationBody(List(NOTE_c, NOTE_e, NOTE_c, WHITESPACE, NOTE_C, NOTE_d, NOTE_B))) {
      parseBody("c2ec C2d/2B//")
    }
  }

  it should "read sharp notes" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcNoteNotation("^B")))) {
      parseBody("A^B")
    }
  }

  it should "read natural notes" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcNoteNotation("=B")))) {
      parseBody("A=B")
    }
  }

  it should "read flat notes" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcNoteNotation("_B")))) {
      parseBody("A_B")
    }
  }

  it should "read ties" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcTieNotation(), NOTE_B))) {
      parseBody("A-B")
    }
  }

  it should "read slurs" in {
    assertResult(AbcNotationBody(List(AbcSlurStartNotation(), AbcChordNotation("A"), NOTE_E, NOTE_e, AbcSlurEndNotation()))) {
      parseBody("(\"A\"Ee)")
    }
  }

  it should "read rests" in {
    assertResult(AbcNotationBody(List(NOTE_A, REST_z, NOTE_B, REST_x))) {
      parseBody("Az2Bx3")
    }
  }

  it should "read multi-measure rests" in {
    assertResult(AbcNotationBody(List(REST_Z))) {
      parseBody("Z4")
    }
  }

  it should "read dotted notes" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcBrokenRythmNotation(">"), NOTE_B))) {
      parseBody("A>B")
    }
    assertResult(AbcNotationBody(List(NOTE_A, AbcBrokenRythmNotation("<"), NOTE_B))) {
      parseBody("A<B")
    }
  }

  it should "read triplets" in {
    assertResult(AbcNotationBody(List(AbcTripletNotation(), NOTE_A, NOTE_B, NOTE_C))) {
      parseBody("(3ABC")
    }
  }

  it should "read chords" in {
    assertResult(AbcNotationBody(List(AbcChordNotation("Em")))) {
      parseBody("\"Em\"")
    }
  }

  it should "read bar markers" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcBarNotation("|"), NOTE_B))) {
      parseBody("A|B")
    }
  }

  it should "read repeat markers" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcRepeatNotation("|:"), NOTE_B,
      NOTE_C, AbcRepeatNotation(":|"), NOTE_D, AbcRepeatNotation(":|]")))) {
      parseBody("A|:BC:|D:|]")
    }
  }

  it should "read unisons" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcUnisonStartNotation(), NOTE_B,
      NOTE_C, AbcUnisonEndNotation()))) {
      parseBody("A[BC]")
    }
  }

  it should "read grace notes" in {
    assertResult(AbcNotationBody(List(AbcGraceStartNotation(), NOTE_A, NOTE_B,
      AbcGraceEndNotation(), NOTE_C))) {
      parseBody("{AB}C")
    }
  }

  it should "read numbered repeat markers" in {
    assertResult(AbcNotationBody(List(NOTE_B, AbcNumberedRepeatNotation("|1"),
      WHITESPACE, NOTE_C, AbcNumberedRepeatNotation(":|2"),
      WHITESPACE, NOTE_D, AbcBarNotation("|"), WHITESPACE,
      AbcNumberedRepeatNotation("[3"), WHITESPACE, NOTE_E))) {
      parseBody("B|1 C:|2 D| [3 E")
    }
  }

  it should "read new lines" in {
    assertResult(AbcNotationBody(List(NOTE_A, BODY_NEWLINE, NOTE_B))) {
      parseBody(
        """A
          |B""".stripMargin
      )
    }
  }

  it should "read body line continuations" in {
    assertResult(AbcNotationBody(List(NOTE_A, BODY_CONTINUATION, BODY_NEWLINE, NOTE_B))) {
      parseBody(
        """A\
          |B""".stripMargin)
    }
  }

  it should "read inline information fields" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcBodyInformationFieldNotation('M', "6/4"), NOTE_B))) {
      parseBody("A[M:6/4]B")
    }
  }

  it should "read inline information fields following bars" in {
    assertResult(AbcNotationBody(List(AbcBarNotation("|"), AbcBodyInformationFieldNotation('M', "6/4")))) {
      parseBody("|[M:6/4]")
    }
  }

  it should "read body comment lines, consuming any whitespace before the comment, and the newline after" in {
    assertResult(AbcNotationBody(List(NOTE_A, BODY_NEWLINE, AbcBodyCommentNotation("body comment"), NOTE_B))) {
      parseBody(
        """A
          | %body comment
          |B""".stripMargin)
    }
  }

  it should "ready body comment lines on the last line of input" in {
    assertResult(AbcNotationBody(List(NOTE_A, BODY_NEWLINE, AbcBodyCommentNotation("body comment")))) {
      parseBody(
        """A
          |%body comment""".stripMargin)
    }
  }

  it should "read a tune's reference" in {
    assertResult(List("1")) {
      headerValue(parseTune(tuneContents), 'X')
    }
  }

  it should "read a tune's key" in {
    assertResult(List("G")) {
      headerValue(parseTune(tuneContents), 'K')
    }
  }

  it should "read multiple tunes" in {
    val tunesNotation = parseTunes(multipleTuneContents)

    assertResult(4) {
      tunesNotation.size
    }
  }

  it should "read Paul Hardy Tunebook" in {
    val pghNotation = parseTunes(pghSessionTunebookContents)

    assertResult(518) {
      pghNotation.size
    }
  }

  implicit val dlwParser = AbcNotationParser.tuneBody

  private def parsing[T](s: String)(implicit p: AbcNotationParser.Parser[T]): T = {
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = AbcNotationParser.phrase(p)
    //we need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(s)
    phraseParser(input) match {
      case AbcNotationParser.Success(t, _) => t
      case AbcNotationParser.NoSuccess(msg, next) => throw new IllegalArgumentException(
        msg + "\nNext is: " + next.pos)
    }
  }

  private def parseBody(s: String): AbcNotationBody = parsing(s)(AbcNotationParser.tuneBody)

  private def parseTune(s: String): AbcTuneNotation = parsing(s)(AbcNotationParser.tune)

  private def parseTunes(s: String): List[AbcTuneNotation] = parsing(s)(AbcNotationParser.tunes)

}
