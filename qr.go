// Copyright 2011 The Go Authors.  All rights reserved.
// Copyright 2024 Vadim Vygonets.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

/*
Package qr encodes QR codes.
*/
package qr // import "github.com/unixdj/qr"

import (
	"image"
	"image/color"
	"strings"

	"github.com/unixdj/qr/coding"
	"github.com/unixdj/qr/split"
)

// A Level denotes a QR error correction level.
// From least to most tolerant of errors, they are L, M, Q, H.
type Level int

const (
	L Level = iota // 20% redundant
	M              // 38% redundant
	Q              // 55% redundant
	H              // 65% redundant
)

// Predefined Charsets.
const (
	UTF8         = split.UTF8         // UTF-8
	UTF8AsLatin1 = split.UTF8AsLatin1 // UTF-8, byte mode in ISO 8859-1
	ShiftJIS     = split.ShiftJIS     // Shift JIS
	ASCIICompat  = split.ASCIICompat  // ASCII-compatible 8 bit encodings
)

// Code formats for EncodeData.
const (
	QR     = split.QR     // QR code
	Micro  = split.Micro  // Micro QR code
	Either = split.Either // Micro QR if data fits, otherwise QR
)

// Extended Channel Interpretation assignment numbers.
const (
	NoECI       = split.NoECI       // No ECI segment
	Latin1ECI   = split.Latin1ECI   // ISO 8859-1
	ShiftJISECI = split.ShiftJISECI // Shift JIS
	UTF8ECI     = split.UTF8ECI     // UTF-8
	BinaryECI   = split.BinaryECI   // 8-bit binary data
)

// Encode returns a QR encoding the text at the given error
// correction level.
func Encode(text string, level Level) (*Code, error) {
	return EncodeData(split.String{Text: text}, level, QR)
}

// EncodeMicro returns a Micro QR encoding of text in the given
// Charset at the given error correction level.  If c is nil, it
// defaults to UTF8.
func EncodeMicro(text string, c split.Charset, level Level) (*Code, error) {
	return EncodeData(split.String{text, c}, level, Micro)
}

// EncodeText returns an encoding of text in the given Charset at the
// given error correction level.  If c is nil, it defaults to UTF8.
// If eci is non-negative, the text is preceded by an ECI mode segment.
func EncodeText(text string, c split.Charset, eci int, level Level) (*Code, error) {
	return EncodeData(split.Text(text, c, eci), level, QR)
}

// EncodeData returns an encoding of data at the given error
// correction level.
func EncodeData(data split.Data, level Level, kind split.Format) (*Code, error) {
	// Split data into segments.
	l := coding.Level(level)
	seg, v, err := split.Split(data, l, kind)
	if err != nil {
		return nil, err
	}
	// Encode the segments.
	cc, err := coding.Encode(v, l, seg...)
	if err != nil {
		return nil, err
	}
	bord := 4
	if cc.Size < 20 {
		bord = 2
	}
	return &Code{cc.Bitmap, cc.Size, cc.Stride, 8, bord, nil, false}, nil
}

// EncodeMulti returns an encoding of data split across multiple QR
// codes with the given version and error correction level.  If header
// is not nil, it is added at the beginning of each code.
func EncodeMulti(header, data split.Data, version coding.Version, level Level) ([]*Code, error) {
	l := coding.Level(level)
	parts, err := split.SplitMulti(header, data, version, l)
	if err != nil {
		return nil, err
	}
	e, err := coding.NewEncoder(version, l)
	if err != nil {
		return nil, err
	}
	c := make([]*Code, len(parts))
	for i := range parts {
		e.Reset()
		cc, err := e.Encode(parts[i]...)
		if err != nil {
			return nil, err
		}
		c[i] = &Code{cc.Bitmap, cc.Size, cc.Stride, 8, 4, nil, false}
	}
	return c, nil
}

// EncodeTextMulti is a combination of EncodeText and EncodeMulti.
// The ECI mode segment is encoded in each code.
func EncodeTextMulti(text string, c split.Charset, eci int, version coding.Version, level Level) ([]*Code, error) {
	return EncodeMulti(split.ShouldSetECI(eci),
		split.String{Text: text, Charset: c}, version, level)
}

// A Code is a square pixel grid.
type Code struct {
	Bitmap  []byte          // 1 is black, 0 is white
	Size    int             // number of pixels on a side
	Stride  int             // number of bytes per row
	Scale   int             // number of image pixels per QR pixel
	Border  int             // number of quiet zone pixels
	Palette *[2]color.Color // background and foreground colors
	Reverse bool            // reverse colors
}

// Black returns true if the pixel at (x,y) is black.
func (c *Code) Black(x, y int) bool {
	var bit byte
	if c.Reverse {
		bit = 1
	}
	if 0 <= x && x < c.Size {
		if p := y*c.Stride + x/8; 0 <= p && p < len(c.Bitmap) {
			bit ^= c.Bitmap[p] >> (7 &^ x)
		}
	}
	return bit&1 != 0
}

// String returns a multiline string containing the code for printing
// on a dark background.  c.Scale and c.Palette are ignored.
func (c *Code) String() string {
	pix := [4]string{"█", "▀", "▄", " "}
	siz := c.Size
	bord := max(c.Border, 0)
	// Allocate 3 bytes per pixel + 1 for newline for each 2 rows.
	var b strings.Builder
	xx := siz + bord*2 + 1
	b.Grow(xx*xx*3/2 - xx)
	var y int
	for y = -bord; y < siz+bord-1; y += 2 {
		for x := -bord; x < siz+bord; x++ {
			var n byte
			if c.Black(x, y) {
				n = 2
			}
			if c.Black(x, y+1) {
				n++
			}
			b.WriteString(pix[n&3])
		}
		b.WriteByte('\n')
	}
	for x := -bord; x < siz+bord; x++ {
		var n byte = 1
		if c.Black(x, y) {
			n = 3
		}
		b.WriteString(pix[n&3])
	}
	b.WriteByte('\n')
	return b.String()
}

type codeImage struct{ *Code }

// Image returns an Image displaying the code.
func (c *Code) Image() image.Image {
	return codeImage{c}
}

var palette = [2]color.Color{color.Black, color.White}

func (c codeImage) Bounds() image.Rectangle {
	d := (c.Size + max(c.Border, 0)*2) * max(c.Scale, 1)
	return image.Rect(0, 0, d, d)
}

func (c codeImage) palette() *[2]color.Color {
	if p := c.Palette; p != nil {
		return p
	}
	return &palette
}

func (c codeImage) ColorModel() color.Model {
	return color.Palette(c.palette()[:])
}

func (c codeImage) ColorIndexAt(x, y int) uint8 {
	scale := max(c.Scale, 1)
	bord := max(c.Border, 0)
	if c.Black(x/scale-bord, y/scale-bord) {
		return 0
	}
	return 1
}

func (c codeImage) At(x, y int) color.Color {
	return c.palette()[c.ColorIndexAt(x, y)]
}

func (c *Code) isValid() bool {
	siz := c.Size
	stride := c.Stride
	bitmap := c.Bitmap
	m := 3
	if siz < 16 {
		m = 1
	}
	v := 8 < siz && siz < 180 && siz&m == 1 && siz/8+1 == stride &&
		len(bitmap) == siz*stride && c.Scale > 0 && c.Border >= 0
	mask := byte(0xff) >> (siz & 7)
	for i := stride - 1; v && i < len(bitmap); i += stride {
		v = bitmap[i]&mask == 0
	}
	return v
}
