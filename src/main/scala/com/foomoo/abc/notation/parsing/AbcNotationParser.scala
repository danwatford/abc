package com.foomoo.abc.notation.parsing

import com.foomoo.abc.notation._

import scala.util.parsing.combinator.RegexParsers

trait AbcNotationParser extends DebugRegexParsers {

  // Don't skip white space as we are interested in empty-lines which separate tunes.
  override val skipWhitespace = false

  // End of input
  def eoi =
    """\z""".r

  def linebreak: Parser[Char] = (literal("\n") | literal("\r\n")) ^^ { case _ => '\n' }

  def nonEscapedCharacter[Char] = not("""\""")

  def emptyLine: Parser[String] = rep1(linebreak) ^^ { case _ => "" }

  def nonLinebreakString: Parser[String] = """[^\r\n]*""".r

  def inlineComment: Parser[String] = "%" ~> nonLinebreakString

  def noteLength: Parser[String] = """[\d]*[/]*[\d]*""".r

  def noteDecorationShortCut: Parser[String] = """[.~HLMOPSTuv]""".r

  def noteDecorationSymbol: Parser[String] = "!" ~> ("""[^!]+""".r <~ "!")

  def noteDecorationSymbolDeprecated: Parser[String] = "+" ~> ("""[^+]+""".r <~ "+")

  def noteDecoration: Parser[String] = noteDecorationShortCut | noteDecorationSymbol | noteDecorationSymbolDeprecated

  def bodyLineContination: Parser[AbcBodyLineContinuation] = """\""" ^^ { case _ => AbcBodyLineContinuation() }

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

  def scoreLineBreak: Parser[AbcBodyScoreLineBreak] = "$" ^^ { case _ => AbcBodyScoreLineBreak() }

  /**
    * The inline information field consists of the open square-bracket immediately followed by an H-W, h-w or plus
    * symbol (+), and then followed by a colon. The information field continues to the close square-bracket, but cannot cross a line break.
    *
    * @return A Parser of AbcBodyInformationFieldNotation
    */
  def tuneBodyInlineInformationField: Parser[AbcBodyInformationFieldNotation] = "[" ~> """[H-Wh-w+]:""".r ~ """[^\]]*""".r <~ "]" ^^ { case fieldKey ~ value => AbcBodyInformationFieldNotation(fieldKey.substring(0, 1), value) }

  /**
    * Parses an AbcBodyInformationFieldNotation at the start of a line based on an H-W, h-w or plus symbol (+)
    * followed by a colon, followed by the field value which runs to the end of the line.
    *
    * The caller should ensure that this parser is only called at the beginning of a line.
    *
    * This parser will consume any line break at the end of the information field. This ensures any line continuation
    * from the previous body line can be applied to the line following this information field line.
    *
    * @return A Parser of AbcBodyInformationFieldNotation.
    */
  def tuneBodyInformationFieldLine: Parser[AbcBodyInformationFieldNotation] =
    """[H-Wh-w+]:""".r ~ nonLinebreakString <~ linebreak ^^ { case fieldKey ~ value => AbcBodyInformationFieldNotation(fieldKey.substring(0, 1), value) }

  /**
    * Parses AbcBodyCommentNotation based on the percent symbol and all following characters up to but not including any line break.
    *
    * The resulting AbcBodyCommentNotation consists of all characters following the percent symbol.
    *
    * @return A Parser of AbcBodyCommentNotation
    */
  def tuneBodyInlineComment: Parser[AbcBodyCommentNotation] = inlineComment ^^ { case comment => AbcBodyCommentNotation(comment) }

  /**
    * Parses an AbcBodyCommentNotation at the start of a line based on any whitespace, followed by the percent
    * symbol, followed by the comment text characters running up to and including any line break.
    *
    * The caller should ensure that this parser is only called at the beginning of a line.
    *
    * The resulting AbcBodyCommentNotation consists of all characters following the percent symbol, but not the
    * line break.
    *
    * This parser will consume any line break at the end of the information field.
    *
    * @return A Parser of AbcBodyCommentNotation
    */
  def tuneBodyLineComment: Parser[AbcBodyCommentNotation] = opt(tuneBodyWhiteSpace) ~> tuneBodyInlineComment <~ opt(linebreak)

  def tuneBodyWhiteSpace: Parser[AbcBodyWhitespaceNotation] = """[ \t]+""".r ^^ { case whitespace => AbcBodyWhitespaceNotation(whitespace) }

  def tuneBodyLineOfElements: Parser[List[AbcNotationBodyElement]] =
    rep1(tuneBodyInlineInformationField | tie | triplet | slurStart | slurEnd | chord | brokenRythm |
      note | rest | numberedRepeatMarker | repeatMarker | barMarker | unisonStart | unisonEnd | graceStart |
      graceEnd | scoreLineBreak | tuneBodyWhiteSpace | tuneBodyInlineComment) ~ opt(bodyLineContination) ~ (linebreak | eoi) ^^ {
      case elementList ~ Some(continuation) ~ '\n' => elementList :+ AbcBodyLineContinuation() :+ AbcBodyNewLine()
      case elementList ~ Some(continuation) ~ _ => elementList :+ AbcBodyLineContinuation()
      case elementList ~ None ~ '\n' => elementList :+ AbcBodyNewLine()
      case elementList ~ None ~ _ => elementList
    }

  def tuneBodyLine: Parser[List[AbcNotationBodyElement]] =
    (tuneBodyLineComment | tuneBodyInformationFieldLine | tuneBodyLineOfElements) ^^ {
      case lineComment: AbcBodyCommentNotation => List(lineComment)
      case informationField: AbcBodyInformationFieldNotation => List(informationField)

        // Use flatMap to convice the compiler (and avoid warnings) that only a list of AbcNotationBodyElements
        // will be returned.
      case elementList: List[Any] => elementList flatMap {
        case element: AbcNotationBodyElement => Some(element)
        case _ => None
      }
    }

  def tuneBody: Parser[AbcNotationBody] = rep(tuneBodyLine) ^^ { case lines => AbcNotationBody(lines.flatten) }

  // An information field value runs to the end of the line. It may contain an inline comment which will need to be
  // stripped out in later processing.
  def informationFieldValue: Parser[String] =
    """[^\r\n]*""".r <~ linebreak

  def refInformationField: Parser[AbcNotationHeaderInformationField] = "X:" ~> informationFieldValue ^^ { case value => AbcNotationHeaderInformationField("X", value) }

  def keyInformationField: Parser[AbcNotationHeaderInformationField] = "K:" ~> informationFieldValue ^^ { case value => AbcNotationHeaderInformationField("K", value) }

  // An information field value runs to the end of the line. It may contain an inline comment which will need to be
  // stripped out in later processing.
  def informationField: Parser[AbcNotationHeaderInformationField] =
    """^[ABCDFGHILMmNOPQRrSTUVWZ+]:""".r ~ informationFieldValue ^^ { case fieldKey ~ values => AbcNotationHeaderInformationField(fieldKey.substring(0, 1), values) }

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