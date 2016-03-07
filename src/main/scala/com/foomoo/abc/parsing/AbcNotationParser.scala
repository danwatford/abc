package com.foomoo.abc.parsing

import com.foomoo.abc._

import scala.util.parsing.combinator.RegexParsers

trait AbcNotationParser extends DebugRegexParsers {

  // Don't skip white space as we are interested in empty-lines which separate tunes.
  override val skipWhitespace = false

  // Detect end of input
  def eoi =
    """\z""".r

  def linebreak: Parser[String] = literal("\n") | literal("\r\n")

  def emptyLine: Parser[String] = rep1(linebreak) ^^ { case _ => "" }

  def nonLinebreakString: Parser[String] = """[^\r\n]*""".r

  def inlineComment: Parser[String] = "%" ~> nonLinebreakString


  def noteLength: Parser[String] = """[\d]*[/]*[\d]*""".r

  def noteDecorationShortCut: Parser[String] = """[.~HLMOPSTuv]""".r

  def noteDecorationSymbol: Parser[String] = "!" ~> ("""[^!]+""".r <~ "!")

  def noteDecorationSymbolDeprecated: Parser[String] = "+" ~> ("""[^+]+""".r <~ "+")

  def noteDecoration: Parser[String] = noteDecorationShortCut | noteDecorationSymbol | noteDecorationSymbolDeprecated

  def bodyLineContination: Parser[AbcBodyLineContinuation] = "\\" <~ linebreak ^^ { case _ => AbcBodyLineContinuation() }

  // A note is zero or many sharp (^) or flat (_) symbols, followed by zero or one natural symbols (=),
  // followed by the note name (a-g, A-G), followed by zero or many octave modifies (',), followed by the note length.
  def note: Parser[AbcNoteNotation] = opt(noteDecoration) ~> ("""[_^]*=?[a-gA-G][',]*""".r <~ noteLength) ^^ { case noteValue => AbcNoteNotation(noteValue) }

  def tie: Parser[AbcTieNotation] = "-" ^^ { case _ => AbcTieNotation() }

  def slurStart: Parser[AbcSlurStartNotation] = "(" ^^ { case _ => AbcSlurStartNotation() }

  def slurEnd: Parser[AbcSlurEndNotation] = ")" ^^ { case _ => AbcSlurEndNotation() }

  def unisonStart: Parser[AbcUnisonStartNotation] = "[" ^^ { case _ => AbcUnisonStartNotation() }

  def unisonEnd: Parser[AbcUnisonEndNotation] = "]" ^^ { case _ => AbcUnisonEndNotation() }

  def graceStart: Parser[AbcGraceStartNotation] = "{" ^^ { case _ => AbcGraceStartNotation() }

  def graceEnd: Parser[AbcGraceEndNotation] = "}" ^^ { case _ => AbcGraceEndNotation() }

  def rest: Parser[AbcRestNotation] = """[Zzx]""".r <~ noteLength ^^ { case rest => AbcRestNotation(rest) }

  def brokenRythm: Parser[AbcBrokenRythmNotation] = (">" | "<") ^^ { case direction => AbcBrokenRythmNotation(direction) }

  def triplet: Parser[AbcTripletNotation] = "(3" ^^ { case _ => AbcTripletNotation() }

  def chord: Parser[AbcChordNotation] = "\"" ~> ("""[^\"]*""".r <~ "\"") ^^ { case chordValue => AbcChordNotation(chordValue) }

  def barMarker: Parser[AbcBarNotation] = opt(noteDecoration) ~> (literal("[|") | literal("|]") | literal("||") | literal("|")) ^^ { case markerString => AbcBarNotation(markerString) }

  def repeatMarker: Parser[AbcRepeatNotation] = opt(noteDecoration) ~> ("[|:" | "|:" | ":|]" | ":|" | "::") ^^ { case markerString => AbcRepeatNotation(markerString) }

  def numberedRepeatMarker: Parser[AbcNumberedRepeatNotation] = (regex("""\|?\[\d""".r) | regex(""":?\|\d""".r)) ^^ { case markerString => AbcNumberedRepeatNotation(markerString) }

  def tuneBodyInlineInformationField: Parser[AbcBodyInlineInformationField] = "[" ~> """[IKLMmNPQRrsTUVWw+]:""".r ~ """[^\r\n\]]*""".r <~ "]" ^^ { case fieldKey ~ value => AbcBodyInlineInformationField(fieldKey.charAt(0), value) }

  def tuneBodyInlineComment: Parser[AbcBodyInlineComment] =
    inlineComment ^^ { case comment => AbcBodyInlineComment(comment) }

  def tuneBodyWhiteSpace: Parser[AbcWhitespaceNotation] = """[ \t]+""".r ^^ { case whitespace => AbcWhitespaceNotation(whitespace) }

  def tuneBodyLineElements: Parser[AbcNotationBodyLineOfElements] =
    rep1(tuneBodyInlineInformationField | tie | triplet | slurStart | slurEnd | chord | brokenRythm |
      note | rest | numberedRepeatMarker | repeatMarker | barMarker | unisonStart | unisonEnd | graceStart |
      graceEnd | tuneBodyWhiteSpace | tuneBodyInlineComment) ~ (bodyLineContination | linebreak | eoi) ^^ {
      case elements ~ AbcBodyLineContinuation() => AbcNotationBodyLineOfElements(elements :+ AbcBodyLineContinuation())
      case elements ~ _ => AbcNotationBodyLineOfElements(elements)
    }

  def tuneBodyLineComment: Parser[AbcNotationBodyLineComment] = "%" ~> nonLinebreakString <~ linebreak ^^ { case comment => AbcNotationBodyLineComment(comment) }

  // An information field value runs to the end of the line. It may contain an inline comment which will need to be
  // stripped out in later processing.
  def tuneBodyInformationField: Parser[AbcNotationBodyInformationField] =
    """[IKLMmNPQRrsTUVWw+]:""".r ~ nonLinebreakString <~ linebreak ^^ { case fieldKey ~ value => AbcNotationBodyInformationField(fieldKey.charAt(0), value) }

  def tuneBody: Parser[AbcNotationBody] =
    rep(tuneBodyLineElements | tuneBodyLineComment | tuneBodyInformationField) ^^ { case lines => AbcNotationBody(lines) }

  // An information field value runs to the end of the line. It may contain an inline comment which will need to be
  // stripped out in later processing.
  def informationFieldValue: Parser[String] =
    """[^\r\n]*""".r <~ linebreak

  def refInformationField: Parser[AbcNotationHeaderInformationField] = "X:" ~> informationFieldValue ^^ { case value => AbcNotationHeaderInformationField('X', value) }

  def keyInformationField: Parser[AbcNotationHeaderInformationField] = "K:" ~> informationFieldValue ^^ { case value => AbcNotationHeaderInformationField('K', value) }

  // An information field value runs to the end of the line. It may contain an inline comment which will need to be
  // stripped out in later processing.
  def informationField: Parser[AbcNotationHeaderInformationField] =
    """^[ABCDFGHILMmNOPQRrSTUVWZ+]:""".r ~ informationFieldValue ^^ { case fieldKey ~ values => AbcNotationHeaderInformationField(fieldKey.charAt(0), values) }

  def tuneHeader: Parser[AbcNotationHeader] =
    refInformationField ~ rep(informationField) ~ keyInformationField ^^ { case refField ~ fieldList ~ keyfield => AbcNotationHeader(refField :: keyfield :: fieldList) }

  def tune: Parser[AbcTuneNotation] = rep(emptyLine) ~> tuneHeader ~ tuneBody ^^ { case headerList ~ bodyList => AbcTuneNotation(headerList, bodyList) }

  def tunes: Parser[List[AbcTuneNotation]] = rep1(tune <~ (rep1(emptyLine) | eoi))
}

object AbcNotationParser extends AbcNotationParser


trait DebugRegexParsers extends RegexParsers {

  class Wrap[+T](name: String, parser: Parser[T]) extends Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      val first = in.first
      val pos = in.pos
      val offset = in.offset
      val t = parser.apply(in)
      println(name + ".apply for token " + first +
        " at position " + pos + " offset " + offset + " returns " + t)
      t
    }
  }

}