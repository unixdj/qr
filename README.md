Go QR encoder package.

go get [-u] github.com/unixdj/qr

Based on rsc.io/qr by Russ Cox.  Drop in replacement for package qr.
The API of package coding is incompatible.

Improvements to qr.Encode:

  * Automatic mask selection
  * Splitting text into segments to minimise encoded length
  * Kanji mode segments
  * Faster encoding

Added features in package qr:

  * Splitting text into multiple QR codes (Structured Append)
  * Printing QR codes as text
  * Prepending an ECI mode segment to set character encoding
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

Packages coding and split:

  * Adding segment encoders
  * Adding input character encodings
