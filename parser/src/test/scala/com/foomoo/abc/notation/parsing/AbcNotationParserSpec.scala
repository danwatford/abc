package com.foomoo.abc.notation.parsing

import java.net.URI

import com.foomoo.abc.notation.AbcNotationTestSupport._
import com.foomoo.abc.notation._

import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

class AbcNotationParserSpec extends UnitSpec {
  val resource: URI = getClass.getResource("/speed_the_plough.abc").toURI
  val multipleTuneResource: URI = getClass.getResource("/multiple_tunes.abc").toURI
  val pghSessionTunebookResource: URI = getClass.getResource("/pgh_session_tunebook.abc").toURI
  val rvw2Resource: URI = getClass.getResource("/rvw2-1.abc").toURI
  val tuneContents: String = Source.fromFile(resource, "UTF-8").mkString
  val multipleTuneContents: String = Source.fromFile(multipleTuneResource, "UTF-8").mkString
  val pghSessionTunebookContents: String = Source.fromFile(pghSessionTunebookResource, "UTF-8").mkString
  val rvw2Contents: String = Source.fromFile(rvw2Resource, "UTF-8").mkString

  "The AbcNotationParser" should "read notes" in {
    assertResult(AbcNotationBody(List(NOTE_c, NOTE_e, NOTE_c, WHITESPACE, NOTE_C, NOTE_d, NOTE_B))) {
      parseBody("c2ec C2d/2B//")
    }
  }

  it should "read sharp notes" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcNotationNote("^B")))) {
      parseBody("A^B")
    }
  }

  it should "read natural notes" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcNotationNote("=B")))) {
      parseBody("A=B")
    }
  }

  it should "read flat notes" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcNotationNote("_B")))) {
      parseBody("A_B")
    }
  }

  it should "read ties" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcNotationTie(), NOTE_B))) {
      parseBody("A-B")
    }
  }

  it should "read slurs" in {
    assertResult(AbcNotationBody(List(AbcNotationSlurStart(), AbcNotationChord("A"), NOTE_E, NOTE_e, AbcNotationSlurEnd()))) {
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
    assertResult(AbcNotationBody(List(NOTE_A, AbcNotationBrokenRhythm(">"), NOTE_B))) {
      parseBody("A>B")
    }
    assertResult(AbcNotationBody(List(NOTE_A, AbcNotationBrokenRhythm("<"), NOTE_B))) {
      parseBody("A<B")
    }
  }

  it should "read triplets" in {
    assertResult(AbcNotationBody(List(AbcNotationTriplet(), NOTE_A, NOTE_B, NOTE_C))) {
      parseBody("(3ABC")
    }
  }

  it should "read chords" in {
    assertResult(AbcNotationBody(List(AbcNotationChord("Em")))) {
      parseBody("\"Em\"")
    }
  }

  it should "read bar markers" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcNotationBar("|"), NOTE_B))) {
      parseBody("A|B")
    }
  }

  it should "read repeat markers" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcNotationRepeat("|:"), NOTE_B,
      NOTE_C, AbcNotationRepeat(":|"), NOTE_D, AbcNotationRepeat(":|]")))) {
      parseBody("A|:BC:|D:|]")
    }
  }

  it should "read unisons" in {
    assertResult(AbcNotationBody(List(NOTE_A, AbcNotationUnisonStart(), NOTE_B,
      NOTE_C, AbcNotationUnisonEnd()))) {
      parseBody("A[BC]")
    }
  }

  it should "read grace notes" in {
    assertResult(AbcNotationBody(List(AbcNotationGraceStart(), NOTE_A, NOTE_B,
      AbcNotationGraceEnd(), NOTE_C))) {
      parseBody("{AB}C")
    }
  }

  it should "read numbered repeat markers" in {
    assertResult(AbcNotationBody(List(NOTE_B, AbcNotationNumberedRepeat(1),
      WHITESPACE, NOTE_C, AbcNotationNumberedRepeat(2),
      WHITESPACE, NOTE_D, AbcNotationBar("|"), WHITESPACE,
      AbcNotationNumberedRepeat(3), WHITESPACE, NOTE_E))) {
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
    assertResult(AbcNotationBody(List(NOTE_A, AbcNotationBodyInformationField("M", "6/4"), NOTE_B))) {
      parseBody("A[M:6/4]B")
    }
  }

  it should "read inline information fields following bars" in {
    assertResult(AbcNotationBody(List(AbcNotationBar("|"), AbcNotationBodyInformationField("M", "6/4")))) {
      parseBody("|[M:6/4]")
    }
  }

  it should "read body comment lines, consuming any whitespace before the comment, and the newline after" in {
    assertResult(AbcNotationBody(List(NOTE_A, BODY_NEWLINE, AbcNotationBodyComment("body comment"), NOTE_B))) {
      parseBody(
        """A
          | %body comment
          |B""".stripMargin)
    }
  }

  it should "read body comment lines on the last line of input" in {
    assertResult(AbcNotationBody(List(NOTE_A, BODY_NEWLINE, AbcNotationBodyComment("body comment")))) {
      parseBody(
        """A
          |%body comment""".stripMargin)
    }
  }

  it should "read $ score line breaks" in {
    assertResult(AbcNotationBody(List(NOTE_A, SCORE_LINEBREAK, NOTE_B, SCORE_LINEBREAK, NOTE_C))) {
      parseBody("A$B$C")
    }
  }

  it should "read a tune's reference" in {
    assertResult(List("1")) {
      headerValue(parseTune(tuneContents), "X")
    }
  }

  it should "read a tune's key" in {
    assertResult(List("G")) {
      headerValue(parseTune(tuneContents), "K")
    }
  }

  it should "read multiple tunes" in {
    val tunesNotation = parseTunes(multipleTuneContents)

    assertResult(4) {
      tunesNotation.size
    }
  }

  it should "read Paul Hardy Tunebook" in {
    inside(parseFile(pghSessionTunebookContents)) {
      case AbcNotationFile(_, _, tuneList) => tuneList should have size 518
    }
  }

  it should "read Vaughan Williams transcription" in {
    inside(parseFile(rvw2Contents)) {
      case AbcNotationFile(_, _, tuneList) => tuneList should have size 3
    }
  }

  it should "read an ABC file without a version string" in {
    inside(parseFile("%abc")) {
      case AbcNotationFile(versionString, _, _) =>
        versionString should be("")
    }
  }

  it should "read an ABC file with a version string" in {
    inside(parseFile("%abc-1.2")) {
      case AbcNotationFile(versionString, _, _) =>
        versionString should be("1.2")
    }
  }

  it should "read an ABC file with a file header" in {
    inside(parseFile(
      """%abc-1.2
        |C:Trad.
        |%%stylesheet directive
        |""".stripMargin)) {
      case AbcNotationFile(_, AbcNotationFileHeader(headerLines), _) =>
        headerLines should contain(AbcNotationHeaderInformationField("C", "Trad."))
    }
  }

  it should "read an ABC file with a file header and tune" in {
    inside(parseFile(
      """%abc
        |C:Trad
        |
        |X:1
        |K:C
        |AB
        |""".stripMargin)) {
      case AbcNotationFile(_, AbcNotationFileHeader(_), tune :: Nil) =>
        inside(tune) {
          case AbcNotationTune(AbcNotationHeader(headerLines), AbcNotationBody(bodyElements)) =>
            headerLines should contain allOf(AbcNotationHeaderInformationField("X", "1"), AbcNotationHeaderInformationField("K", "C"))
            bodyElements should contain allOf(NOTE_A, NOTE_B)
        }
    }
  }

  it should "read an ABC file without a file header" in {
    inside(parseFile(
      """%abc
        |
        |X:1
        |K:C
        |AB
      """.stripMargin)) {
      case AbcNotationFile(_, AbcNotationFileHeader(headerLines), tuneList) =>
        headerLines shouldBe empty
        tuneList should have size 1
    }
  }

  it should "read an ABC file with empty lines at the end" in {
    inside(parseFile(
      """%abc
        |
        |X:1
        |K:C
        |AB
        |
        |
        |""".stripMargin)) {
      case AbcNotationFile(_, _, tuneList) =>
        tuneList should have size 1
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

  private def parseTune(s: String): AbcNotationTune = parsing(s)(AbcNotationParser.tune)

  private def parseTunes(s: String): List[AbcNotationTune] = parsing(s)(AbcNotationParser.tunes)

  private def parseFile(s: String): AbcNotationFile = parsing(s)(AbcNotationParser.file)

}
