package com.foomoo.abc.parsing

import java.net.URI

import com.foomoo.abc._

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
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("c"), AbcNoteNotation("e"), AbcNoteNotation("c"), AbcWhitespaceNotation(" "), AbcNoteNotation("C"), AbcNoteNotation("d"), AbcNoteNotation("B")))) {
      parseBodyLine("c2ec C2d/2B//")
    }
  }

  it should "read sharp notes" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcNoteNotation("^B")))) {
      parseBodyLine("A^B")
    }
  }

  it should "read natural notes" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcNoteNotation("=B")))) {
      parseBodyLine("A=B")
    }
  }

  it should "read flat notes" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcNoteNotation("_B")))) {
      parseBodyLine("A_B")
    }
  }

  it should "read ties" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcTieNotation(), AbcNoteNotation("B")))) {
      parseBodyLine("A-B")
    }
  }

  it should "read slurs" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcSlurStartNotation(), AbcChordNotation("A"), AbcNoteNotation("E"), AbcNoteNotation("e"), AbcSlurEndNotation()))) {
      parseBodyLine("(\"A\"Ee)")
    }
  }

  it should "read rests" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcRestNotation("z"), AbcNoteNotation("B"), AbcRestNotation("x")))) {
      parseBodyLine("Az2Bx3")
    }
  }

  it should "read multi-measure rests" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcRestNotation("Z")))) {
      parseBodyLine("Z4")
    }
  }

  it should "read dotted notes" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcBrokenRythmNotation(">"), AbcNoteNotation("B")))) {
      parseBodyLine("A>B")
    }
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcBrokenRythmNotation("<"), AbcNoteNotation("B")))) {
      parseBodyLine("A<B")
    }
  }

  it should "read triplets" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcTripletNotation(), AbcNoteNotation("F"), AbcNoteNotation("G"), AbcNoteNotation("A")))) {
      parseBodyLine("(3FGA")
    }
  }

  it should "read chords" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcChordNotation("Em")))) {
      parseBodyLine("\"Em\"")
    }
  }

  it should "read bar markers" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcBarNotation("|"), AbcNoteNotation("B")))) {
      parseBodyLine("A|B")
    }
  }

  it should "read repeat markers" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcRepeatNotation("|:"), AbcNoteNotation("B"),
      AbcNoteNotation("C"), AbcRepeatNotation(":|"), AbcNoteNotation("D"), AbcRepeatNotation(":|]")))) {
      parseBodyLine("A|:BC:|D:|]")
    }
  }

  it should "read unisons" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcUnisonStartNotation(), AbcNoteNotation("B"),
      AbcNoteNotation("C"), AbcUnisonEndNotation()))) {
      parseBodyLine("A[BC]")
    }
  }

  it should "read grace notes" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcGraceStartNotation(), AbcNoteNotation("A"), AbcNoteNotation("B"),
      AbcGraceEndNotation(), AbcNoteNotation("C")))) {
      parseBodyLine("{AB}C")
    }
  }

  it should "read numbered repeat markers" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("B"), AbcNumberedRepeatNotation("|1"),
      AbcWhitespaceNotation(" "), AbcNoteNotation("C"), AbcNumberedRepeatNotation(":|2"),
      AbcWhitespaceNotation(" "), AbcNoteNotation("D"), AbcBarNotation("|"), AbcWhitespaceNotation(" "),
      AbcNumberedRepeatNotation("[3"), AbcWhitespaceNotation(" "), AbcNoteNotation("E")))) {
      parseBodyLine("B|1 C:|2 D| [3 E")
    }
  }

  it should "read body line continuations" in {
    assertResult(AbcNotationBody(List(
      AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcBodyLineContinuation())),
      AbcNotationBodyLineOfElements(List(AbcNoteNotation("B")))
    ))) {
      parseBody(
        """A\
          |B""".stripMargin)
    }
  }
  it should "read inline information fields" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcNoteNotation("A"), AbcBodyInlineInformationField('M', "6/4"), AbcNoteNotation("B")))) {
      parseBodyLine("A[M:6/4]B")
    }
  }

  it should "read inline information fields following bars" in {
    assertResult(AbcNotationBodyLineOfElements(List(AbcBarNotation("|"), AbcBodyInlineInformationField('M', "6/4")))) {
      parseBodyLine("|[M:6/4]")
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

  private def headerValue(tuneNotation: AbcTuneNotation, key: Char): List[String] = tuneNotation match {
    case AbcTuneNotation(AbcNotationHeader(headerList), _) =>
      headerList.filter {
        case AbcNotationHeaderInformationField(headerKey, _) => headerKey == key
        case _ => false
      } map {
        case AbcNotationHeaderInformationField(_, headerValue) => headerValue
        case _ => throw new IllegalArgumentException("Only AbcNotationHeaderInformation expected")
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

  private def parseBodyLine(s: String): AbcNotationBodyLineOfElements = parsing(s)(AbcNotationParser.tuneBodyLineElements)

  private def parseBody(s: String): AbcNotationBody = parsing(s)(AbcNotationParser.tuneBody)

  private def parseTune(s: String): AbcTuneNotation = parsing(s)(AbcNotationParser.tune)

  private def parseTunes(s: String): List[AbcTuneNotation] = parsing(s)(AbcNotationParser.tunes)

}
