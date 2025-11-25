QR encoder Go package and command line tool.

go get [-u] github.com/unixdj/qr

Based on rsc.io/qr by Russ Cox.  Drop in replacement for package qr.
The API of package coding is incompatible.

Command line tool:

  * All of the encoding options listed below
  * Output: PNG using bespoke and standard Go encoders, PBM,
    Encapsulated PostScript, UTF-8 and ASCII text
  * Rotating and reflecting codes
  * Encoding entire data in byte mode
  * Disabling Kanji mode
  * True 8 bit byte mode for structured append splitting
  * Shift JIS input with ISO 8859-1 byte mode
  * Conversion to upper case for shorter encoded length

Improvements to qr.Encode:

  * Automatic mask selection
  * Splitting text into segments to minimise encoded length
  * Kanji mode segments
  * Faster encoding

Added features in package qr:

  * Splitting text into multiple QR codes (Structured Append)
  * Micro QR codes
  * Printing codes as UTF-8 text
  * Prepending an ECI mode segment to set character encoding
  * FNC1 mode support
  * Character encodings:
    * UTF-8 (default)
    * UTF-8 with byte mode segments encoded as ISO 8859-1
    * Shift JIS, Shift JISx0213 (Shift JIS-2004)
    * ASCII-compatible eight bit encodings
  * Image encoding:
    * Faster and more efficient PNG encoder
    * Encoding PNG into io.Writer, additionally to byte slice
    * PBM encoder (side effect of PNG encoding implementation)
    * Border (quiet zone) size configuration
    * Reverse colours (also for text printing)
    * Custom colour palette (PNG/EncodePNG and Image only)

Encoding customisation (packages coding and split):

  * Adding segment encoders
  * Adding input character encodings
