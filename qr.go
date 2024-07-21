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

// Extended Channel Interpretation assignment numbers.
const (
	Latin1ECI   = split.Latin1ECI   // ISO 8859-1
	ShiftJISECI = split.ShiftJISECI // Shift JIS
	UTF8ECI     = split.UTF8ECI     // UTF-8
	BinaryECI   = split.BinaryECI   // 8-bit binary data
)

// Encode returns an encoding of text at the given error correction
// level.
func Encode(text string, level Level) (*Code, error) {
	return EncodeData(split.String{Text: text}, level)
}

// EncodeText returns an encoding of text in the given Charset at the
// given error correction level.  If the Charset is nil, it defaults
// to UTF8.  If eci is not 0, the text is preceded by an ECI mode
// segment.
func EncodeText(text string, c split.Charset, eci uint32, level Level) (*Code, error) {
	return EncodeData(split.Text(text, c, eci), level)
}

// EncodeData returns an encoding of data at the given error
// correction level.
func EncodeData(data split.Data, level Level) (*Code, error) {
	// Split data into segments.
	l := coding.Level(level)
	seg, v, err := split.Split(data, l)
	if err != nil {
		return nil, err
	}
	// Encode the segments.
	cc, err := coding.Encode(v, l, seg...)
	if err != nil {
		return nil, err
	}
	return &Code{cc.Bitmap, cc.Size, cc.Stride, 8}, nil
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
		c[i] = &Code{cc.Bitmap, cc.Size, cc.Stride, 8}
	}
	return c, nil
}

// EncodeTextMulti is a combination of EncodeText and EncodeMulti.
// The ECI mode segment is encoded in each code.
func EncodeTextMulti(text string, c split.Charset, eci uint32, version coding.Version, level Level) ([]*Code, error) {
	return EncodeMulti(split.ShouldSetECI(eci),
		split.String{Text: text, Charset: c}, version, level)
}

// A Code is a square pixel grid.
// It implements image.Image and direct PNG encoding.
type Code struct {
	Bitmap []byte // 1 is black, 0 is white
	Size   int    // number of pixels on a side
	Stride int    // number of bytes per row
	Scale  int    // number of image pixels per QR pixel
}

// Black returns true if the pixel at (x,y) is black.
func (c *Code) Black(x, y int) bool {
	return 0 <= x && x < c.Size && 0 <= y && y < c.Size &&
		c.Bitmap[y*c.Stride+x/8]&(1<<uint(7&^x)) != 0
}

// Reverse returns a code with colors reversed.
func (c *Code) Reverse() Reversed { return Reversed{c} }

// Reversed is a Code with colors reversed.
type Reversed struct {
	*Code
}

// Black returns true if the original pixel at (x,y) is white.
func (c Reversed) Black(x, y int) bool { return !c.Code.Black(x, y) }

// String returns a multiline string containing the code for printing
// on a dark background.
func (c *Code) String() string { return c.string(0) }

// String returns a multiline string containing the code for printing
// on a light background.
func (c Reversed) String() string { return c.string(3) }

func (c *Code) string(inv byte) string {
	pix := [4]string{"█", "▀", "▄", " "}
	siz := c.Size
	// Allocate 3 bytes per pixel + 1 for newline for each 2 rows.
	var b strings.Builder
	xx := siz + 9
	b.Grow(3*xx*xx/2 - xx)
	for y := -4; y < siz+3; y += 2 {
		for x := -4; x < siz+4; x++ {
			var n byte
			if c.Black(x, y) {
				n = 2
			}
			if c.Black(x, y+1) {
				n++
			}
			b.WriteString(pix[(n^inv)&3])
		}
		b.WriteByte('\n')
	}
	s := pix[(inv|1)&3]
	for x := -4; x < siz+4; x++ {
		b.WriteString(s)
	}
	b.WriteByte('\n')
	return b.String()
}

// Image returns an Image displaying the code.
func (c *Code) Image() image.Image {
	return &codeImage{c}

}

// Image returns an Image displaying the reversed code.
func (c Reversed) Image() image.Image {
	return &reverseImage{codeImage{c.Code}}
}

// codeImage implements image.Image
type codeImage struct {
	*Code
}

// reverseImage implements image.Image
type reverseImage struct {
	codeImage
}

var palette = color.Palette{color.Black, color.White}

func (c *codeImage) Bounds() image.Rectangle {
	d := (c.Size + 8) * c.Scale
	return image.Rect(0, 0, d, d)
}

func (c *codeImage) ColorModel() color.Model {
	return palette
}

func (c *codeImage) ColorIndexAt(x, y int) uint8 {
	if c.Black(x/c.Scale-4, y/c.Scale-4) {
		return 0
	}
	return 1
}

func (c *codeImage) At(x, y int) color.Color {
	return palette[c.ColorIndexAt(x, y)]
}

func (c *reverseImage) ColorIndexAt(x, y int) uint8 {
	if c.Black(x/c.Scale-4, y/c.Scale-4) {
		return 1
	}
	return 0
}

func (c *reverseImage) At(x, y int) color.Color {
	return palette[c.ColorIndexAt(x, y)]
}
