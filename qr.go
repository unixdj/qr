// Copyright 2011 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

/*
Package qr encodes QR codes.
*/
package qr // import "rsc.io/qr"

import (
	"errors"
	"image"
	"image/color"

	"rsc.io/qr/coding"
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

var sizeClass = [3]struct {
	min, max coding.Version
}{
	{1, 9}, {10, 26}, {27, 40},
}

const (
	numMode    = iota // numeric
	alphaMode         // alphanumeric
	kanjiMode         // kanji
	stringMode        // byte
	modes             // total number of modes

	numModes    = 1<<numMode | 1<<alphaMode | 1<<stringMode
	alphaModes  = 1<<alphaMode | 1<<stringMode
	kanjiModes  = 1<<stringMode | 1<<kanjiMode
	stringModes = 1 << stringMode
)

// bits[m] returns segment size in bits for a string of
// n bytes, k kanji at QR version size class class encoded in mode m.
var bits = [modes]func(n, k, class int) int{
	func(n, k, class int) int { return 14 + class*2 + (10*n+2)/3 },
	func(n, k, class int) int { return 13 + class*2 + (11*n+1)/2 },
	func(n, k, class int) int { return 12 + class*2 + k*13 },
	func(n, k, class int) int { return 12 + (class<<1>>class+n)*8 },
}

type (
	// segment describes a segment encoded in a certain mode.
	segment struct {
		next   *segment // link to next segment in the chain
		start  int      // start of string
		slen   int      // length of string in bytes
		klen   int      // length of string in kanji
		weight int      // encoded size of all segments in the chain
		mode   byte     // encoding mode
	}

	// span describes a span of bytes encodable in the same modes.
	span struct {
		start int            // start of string
		slen  int            // length of string in bytes
		klen  int            // length of string in kanji
		modes byte           // bit field of valid encoding modes
		seg   [modes]segment // segments
	}
)

// classify splits text into spans of bytes encidable in the same modes.
func classify(text string) []span {
	if text == "" {
		return nil
	}
	const (
		alpha = 0x07ff_fffe_07ff_ec31 // SPACE $% *+ -./ [0-9] : [A-Z]
		digit = 0x0000_0000_03ff_0000 // [0-9]
	)

	// Scan the string, detect valid encoding modes for each byte
	modes := make([]byte, len(text))
	common := ^byte(0) // bit field of modes common to all spans
	n := 0
	m := byte(0)
	for i, r := range text {
		old := m
		m = stringModes
		if bit := uint64(1) << (uint(r) - ' '); digit&bit != 0 {
			m = numModes
		} else if alpha&bit != 0 {
			m = alphaModes
		} else if jis0208.Contains(r) {
			m = kanjiModes
		}
		modes[i] = m
		if m != old {
			common &= m
			n++
		}
	}

	mask := ^common | -common // Mask common modes except the lowest

	// Set spans
	sp := make([]span, n)
	old, n := byte(0), 0
	for i, v := range modes {
		if v != 0 && v != old {
			if i != 0 {
				sp[n].slen = i - sp[n].start
				n++
			}
			sp[n].start = i
			sp[n].modes = v & mask
			old = v
		}
		if v == kanjiModes && text[i] >= 0xc0 {
			sp[n].klen++
		}
	}
	sp[n].slen = len(modes) - sp[n].start
	return sp
}

/*
split returns the optimal split for the string described by sp at
the given QR version size class.

For last span, for each valid mode j:
  - Create a segment sp[len(sp)-1].seg[j] describing the span
    encoded in mode j.  Calculate the weight (encoded length in
    bits).

Then walk backwards through the rest of the spans.
For each span i, for each valid mode j:
  - For each mode k valid for span i+1, create a segment linking
    to next=sp[i+1].seg[k].  If k==j, merge the segments by
    adding the length of next and linking to next.next instead.
    Calculate the weight of the segment.  If next is not nil, add
    the weight of next to get the combined weight of the chain.
  - From those segments choose the one with the smallest weight.
    Assign it to sp[i].seg[j].

Return the address of the segment in sp[0].seg with the smallest
weight.
*/
func split(sp []span, class int) *segment {
	const Inf = 1 << 30
	// Process last span.  Create a segment for each valid mode.
	i := len(sp) - 1
	if i < 0 {
		return nil
	}
	for j := byte(0); j < modes; j++ {
		seg := &sp[i].seg[j]
		*seg = segment{weight: Inf}
		if sp[i].modes>>j&1 != 0 {
			*seg = segment{
				start:  sp[i].start,
				slen:   sp[i].slen,
				klen:   sp[i].klen,
				weight: bits[j](sp[i].slen, sp[i].klen, class),
				mode:   byte(j),
			}
			if i == 0 {
				return seg
			}
		}
	}

	// Process the rest of the spans.
	for i--; i >= 0; i-- {
		v := &sp[i]
		for j := byte(0); j < modes; j++ {
			seg := &v.seg[j]
			*seg = segment{weight: Inf}
			if v.modes>>j&1 == 0 {
				continue
			}
			weight := bits[j](v.slen, v.klen, class)
			ns := &sp[i+1].seg
			for k := byte(0); k < modes; k++ {
				next := &ns[k]
				if next.weight == Inf {
					continue
				}
				c := segment{
					next:   next,
					start:  v.start,
					slen:   v.slen,
					klen:   v.klen,
					weight: weight,
					mode:   j,
				}
				if k == j {
					c.slen += c.next.slen
					c.next = c.next.next
					c.weight = bits[j](c.slen, 0, class)
				}
				if c.next != nil {
					c.weight += c.next.weight
				}
				if c.weight < seg.weight {
					*seg = c
				}
			}
		}
	}

	// Choose the first segment with the smallest weight
	seg := &sp[0].seg[0]
	for j := 1; j < modes; j++ {
		if sp[0].seg[j].weight < seg.weight {
			seg = &sp[0].seg[j]
		}
	}
	return seg
}

// Encode returns an encoding of text at the given error correction level.
func Encode(text string, level Level) (*Code, error) {
	l := coding.Level(level)
	// Estimate minimum QR version size class in a crude manner.
	class := 0
	weight := bits[0](len(text), 0, class)
	for class < 2 && sizeClass[class].max.DataBytes(l)*8 < weight {
		class++
	}
	// Split string into spans.
	sp := classify(text)
	// Split string into segments for the size class.
	seg := split(sp, class)
	if seg != nil { // seg is nil if text == ""
		weight = seg.weight
	}
	// If string is too big for the size class, increment class
	// and resplit.  The weight will change, hence the loop.
	for sizeClass[class].max.DataBytes(l)*8 < weight {
		class++
		for class < 3 && sizeClass[class].max.DataBytes(l)*8 < weight {
			class++
		}
		if class == 3 {
			return nil, errors.New("text too long to encode as QR")
		}
		seg = split(sp, class)
		weight = seg.weight
	}

	// Find version in the size class.
	v := sizeClass[class].min
	for max := sizeClass[class].max; v < max; {
		if mid := (v + max) / 2; mid.DataBytes(l)*8 < weight {
			v = mid + 1
		} else {
			max = mid
		}
	}

	// Count and encode the segments.
	n := 0
	for s := seg; s != nil; s = s.next {
		n++
	}
	enc := make([]coding.Encoding, 0, n)
	for seg != nil {
		var e coding.Encoding
		s := text[seg.start : seg.start+seg.slen]
		switch seg.mode {
		case numMode:
			e = coding.Num(s)
		case alphaMode:
			e = coding.Alpha(s)
		case kanjiMode:
			e = coding.Kanji(s)
		default:
			e = coding.String(s)
		}
		enc = append(enc, e)
		seg = seg.next
	}

	// Build and execute plan.
	cc, err := coding.Encode(v, l, enc...)
	if err != nil {
		return nil, err
	}

	return &Code{cc.Bitmap, cc.Size, cc.Stride, 8}, nil
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
		c.Bitmap[y*c.Stride+x/8]&(1<<uint(7-x&7)) != 0
}

// Image returns an Image displaying the code.
func (c *Code) Image() image.Image {
	return &codeImage{c}

}

// codeImage implements image.Image
type codeImage struct {
	*Code
}

var (
	whiteColor color.Color = color.Gray{0xFF}
	blackColor color.Color = color.Gray{0x00}
)

func (c *codeImage) Bounds() image.Rectangle {
	d := (c.Size + 8) * c.Scale
	return image.Rect(0, 0, d, d)
}

func (c *codeImage) At(x, y int) color.Color {
	if c.Black(x, y) {
		return blackColor
	}
	return whiteColor
}

func (c *codeImage) ColorModel() color.Model {
	return color.GrayModel
}
