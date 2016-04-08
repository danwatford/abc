# ABC Notation Parser
Parser and processor for tunes written in ABC Notation. Based on information from http://abcnotation.com/learn

This library contains a parser and serializer for ABC Notation. It also contains some basic processing to remove
comments, line continuations and to extract note sequences. The original intent of the library was to provide
a way to extract notes from ABC tunes in order to find matching note sequences across tunes.

An example TuneMatcher program has been provided to demonstrate finding note sequences common to tunes. This
can be built and run from an sbt against the ABC tune files included in the test resources using:
    ```sbt
    run src/test/resources/pgh_session_tunebook.abc src/test/resources/rvw2-1.abc src/test/resources/pgh_possible_tunebook.abc src/test/resources/pgh_annex_tunebook.abc
    ```

Tunes taken from Paul Hardy Tunebooks: http://www.pghardy.net/concertina/tunebooks/ and
Full English Transcription Programme: http://folkopedia.efdss.org/wiki/Full_English_Transcription_Programme:_The_Vaughan_Williams_Archive_at_the_British_Library,_MS_RVW2/1
