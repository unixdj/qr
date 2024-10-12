// Copyright 2011 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package split_test

import (
	"bytes"
	"fmt"
	"log"
	"unicode/utf16"
	"unicode/utf8"

	"golang.org/x/text/encoding/japanese"
	"golang.org/x/text/encoding/unicode"
	"golang.org/x/text/transform"

	"github.com/unixdj/qr"
	"github.com/unixdj/qr/coding"
	"github.com/unixdj/qr/split"
)

// ECI assignment number
const UTF16BEECI = 25 // UTF-16, big endian

var utf16Encoding = unicode.UTF16(unicode.BigEndian, unicode.IgnoreBOM)

// utf16DecodePair decodes two bytes as a UTF-16BE rune.  The rune may
// be a surrogate.
func utf16DecodePair(s string) (rune, int) {
	if len(s) < 2 {
		return utf8.RuneError, len(s)
	}
	return rune(s[0])<<8 | rune(s[1]), 2
}

// utf16DecodeRune decodes a UTF-16BE rune.
func utf16DecodeRune(s string) (rune, int) {
	r, n := utf16DecodePair(s)
	if utf16.IsSurrogate(r) {
		r2, _ := utf16DecodePair(s[2:])
		if r = utf16.DecodeRune(r, r2); r != utf8.RuneError {
			n = 4
		}
	}
	return r, n
}

// utf16Alnum creates a UTF-16BE numeric or alphanumeric mode encoder.
func utf16Alnum(mode coding.Mode) coding.Mode {
	m := coding.GetMode(mode)
	// Use original CountLength and Accepts.
	m.Name = "utf16-" + m.Name
	// EncodedLength calls the original with half the byte count.
	el := m.EncodedLength
	m.EncodedLength = func(b, r int) int { return el(b/2, r) }
	// CutRune decodes a UTF-16 rune.
	m.CutRune = utf16DecodeRune
	// Transform returns a segment of the original mode.
	m.Transform = func(s string) (coding.Segment, bool) {
		// s is pre-validated, convert from UTF-16BE to UTF-8/ASCII.
		b := make([]byte, len(s)/2)
		for i := 1; i < len(s); i += 2 {
			b[i/2] = s[i]
		}
		return coding.Segment{string(b), mode}, true
	}
	return coding.AddMode(m)
}

var (
	utf16Num   = utf16Alnum(coding.Numeric)
	utf16Alpha = utf16Alnum(coding.Alphanumeric)
	utf16Kanji = func() coding.Mode {
		m := coding.GetMode(coding.ShiftJISKanji)
		// Use original CountLength and EncodedLength.
		// The latter is compatible, as both UTF-16 and Shift JIS
		// encode QR Kanji characters as two bytes.
		m.Name = "utf16-kanji"
		m.CutRune = utf16DecodeRune
		m.Accepts = coding.IsKanji
		// Transform converts UTF-16BE to Shift JIS and returns
		// a ShiftJISKanji segment.
		m.Transform = func(s string) (coding.Segment, bool) {
			d := utf16Encoding.NewDecoder()
			e := japanese.ShiftJIS.NewEncoder()
			t, _, err := transform.String(transform.Chain(d, e), s)
			return coding.Segment{t, coding.ShiftJISKanji},
				err == nil
		}
		return coding.AddMode(m)
	}()

	// Create the UTF-16BE split.Charset using the modes above and
	// standard byte mode.  UTF-16 encodes ASCII digits as 2 bytes.
	utf16be = split.NewCharset(split.ModeList{
		utf16Num, utf16Alpha, utf16Kanji, coding.Byte}, 2)
)

func ExampleNewCharset() {
	// Create a UTF-16BE string
	s, err := utf16Encoding.NewEncoder().String("- [123;45] 20円")
	if err != nil {
		log.Fatalln(err)
	}

	// Encode the string preceded by an ECI segment and print the QR code
	c, err := qr.EncodeText(s, utf16be, UTF16BEECI, qr.M)
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Print(c)

	// Demo: Split the string and print the segments.  The string above
	// demonstrates segments of all four modes and merged segments.
	seg, v, err := split.Split(split.Text(s, utf16be, UTF16BEECI),
		coding.M)
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Printf("version %d, %d segments:\n", v, len(seg))
	class := v.SizeClass()
	utf16Dec := utf16Encoding.NewDecoder()
	sjisDec := japanese.ShiftJIS.NewDecoder()
	for i := range seg {
		// Transform the segment
		t, err := seg[i].Transform()
		if err != nil {
			log.Fatalln(err)
		}
		// Convert the string to UTF-8 for printing
		s := t.Text
		switch t.Mode {
		case coding.Byte:
			s, err = utf16Dec.String(t.Text)
			if err != nil {
				log.Fatalln(err)
			}
		case coding.ShiftJISKanji:
			s, err = sjisDec.String(s)
			if err != nil {
				log.Fatalln(err)
			}
		}
		// Print mode before and after Transform, string length
		// in bytes (2 bytes per rune in Byte and ShiftJISKanji,
		// 1 byte otherwise), encoded length in bits and text.
		fmt.Printf("  %-18s -> %-15s %2d bytes %3d bits  ",
			seg[i].Mode, t.Mode, len(t.Text),
			t.EncodedLength(class))
		if t.Mode == coding.ECI && len(t.Text) == 1 {
			fmt.Printf("%#02x\n", s[0])
		} else {
			fmt.Printf("%q\n", s)
		}
	}

	// To encode the segments above:
	cc, err := coding.Encode(v, coding.M, seg...)
	if err != nil {
		log.Fatalln(err)
	}
	c2 := &qr.Code{
		Bitmap: cc.Bitmap,
		Size:   cc.Size,
		Stride: cc.Stride,
		Scale:  8,
	}
	if !bytes.Equal(c.Bitmap, c2.Bitmap) {
		log.Fatalln("encoding differs")
	}
}
