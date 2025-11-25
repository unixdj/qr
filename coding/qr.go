// Copyright 2011 The Go Authors.  All rights reserved.
// Copyright 2024 Vadim Vygonets.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package coding implements low-level QR and Micro QR coding details.
package coding // import "github.com/unixdj/qr/coding"

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"unicode/utf8"

	"github.com/unixdj/qr/gf256"
	"golang.org/x/text/encoding/charmap"
	"golang.org/x/text/encoding/japanese"
)

var (
	ErrLevel   = errors.New("qr: invalid level")
	ErrVersion = errors.New("qr: invalid version")
)

// Field is the field for QR error correction.
var Field = gf256.NewField(0x11d, 2)

// A Version represents a QR version.
// The version specifies the size of the QR code:
// a QR code with version v has 4v+17 pixels on a side,
// a Micro QR code with version Mv 2v+9 pixels.
// Versions run in two sequences, from M1 to M4 and from 1 to 40:
// the larger the version, the more information the code can store.
type Version int

// Code versions.
const (
	// Micro QR versions
	M1 Version = MaxVersion + 1 + iota
	M2
	M3
	M4

	MinVersion Version = 1  // Minimum QR version
	MaxVersion Version = 40 // Maximum QR version
)

func (v Version) String() string {
	if v >= M1 {
		return []string{"M1", "M2", "M3", "M4"}[v-M1]
	}
	return strconv.Itoa(int(v))
}

// Micro QR and QR version size classes.
const (
	ClassM1 = iota // Micro QR version M1
	ClassM2        // Micro QR version M2
	ClassM3        // Micro QR version M3
	ClassM4        // Micro QR version M4
	Class0         // QR versions 1 to 9
	Class1         // QR versions 10 to 26
	Class2         // QR versions 27 to 40
)

// SizeClass returns the size class of v, as documented under ClassM1.
func (v Version) SizeClass() int {
	if v <= 9 {
		return Class0
	}
	if v <= 26 {
		return Class1
	}
	if v <= 40 {
		return Class2
	}
	return int(v - M1)
}

// dataBytes returns the number of data bytes that can be
// stored in a QR code with the given version and level.
func (v Version) dataBytes(l Level) int {
	vt := &vtab[v]
	lev := vt.level[l]
	return vt.bytes - lev.nblock*lev.check
}

// DataBits returns the number of data bits that can be
// stored in a QR code with the given version and level.
func (v Version) DataBits(l Level) int {
	n := v.dataBytes(l) * 8
	if v >= M1 && n != 0 {
		n -= int(v) & 1 << 2
	}
	return n
}

type Bits struct {
	b    []byte
	nbit int
}

// NewBits returns Bits with enough capacity for a QR code of the
// given version and level.
func NewBits(v Version, l Level) *Bits {
	vt := &vtab[v]
	n := vt.bytes
	if 1 < vt.level[l].nblock {
		n <<= 1
	}
	return &Bits{b: make([]byte, 0, n)}
}

func (b *Bits) Reset() {
	b.b = b.b[:0]
	b.nbit = 0
}

func (b *Bits) Bits() int {
	return b.nbit
}

func (b *Bits) Bytes() []byte {
	if b.nbit%8 != 0 {
		panic("qr: fractional byte")
	}
	return b.b
}

func (b *Bits) growTo(n int) {
	for cap(b.b) < n {
		b.b = append(b.b[:cap(b.b)], 0)[:len(b.b)]
	}
}

func (b *Bits) Grow(n int) { b.growTo(len(b.b) + n) }

// Add adds n bytes to b and returns the added slice.
func (b *Bits) Add(n int) []byte {
	if b.nbit%8 != 0 {
		panic("qr: fractional byte")
	}
	b.Grow(n)
	start := len(b.b)
	b.b = b.b[:start+n]
	b.nbit = 8 * len(b.b)
	return b.b[start:]
}

func (b *Bits) Write(v uint32, nbit int) {
	v <<= 32 - nbit
	if rem := -b.nbit & 7; rem != 0 {
		b.b[len(b.b)-1] |= byte(v >> (32 - rem))
		if rem >= nbit {
			b.nbit += nbit
			return
		}
		b.nbit += rem
		nbit -= rem
		v <<= rem
	}
	for n := nbit; n > 0; n -= 8 {
		b.b = append(b.b, byte(v>>24))
		v <<= 8
	}
	b.nbit += nbit
}

// Predefined encoding modes.
const (
	Numeric       Mode = iota // numeric mode, ASCII-compatible text
	Alphanumeric              // alphanumeric mode, ASCII-compatible text
	Byte                      // byte mode, any data
	Kanji                     // kanji mode, UTF-8 text
	Latin1                    // byte mode, UTF-8 text encoded as ISO 8859-1
	ShiftJISKanji             // kanji mode, Shift JIS text
	FNC1Alpha                 // alphanumeric mode for FNC1 codes
	ECI                       // eci mode, raw segment
	StructAppend              // structured append, raw segment
	FNC1First                 // FNC1 in 1st position
	FNC1Second                // FNC1 in 2nd position
)

// A Mode is a QR segment encoder.
type Mode int16

// ModeEncoder implements a QR segment encoding.
//
// The segment is validated using either Valid or CutRune and Accepts.
// Text mode encoders other than Numeric, Alphanumeric, Byte and
// ShiftJISKanji must have a Transform function returning a segment of
// one of those modes.  If set, it is called by Segment.Transform
// after validation.  The encoder calls Segment.Transform and
// validates the returned segment before encoding.
//
// Package split uses Indicator to determine valid Micro QR versions;
// CutRune, Accepts, EncodedLength and CountLength to split text into
// segments; and Transform for calculating the checksum when splitting
// input into multiple QR codes (Structured Append).
//
// Name, Indicator and CountLength must be set.
type ModeEncoder struct {
	Name      string // Name for error reporting
	Indicator byte   // 4 bit mode indicator for QR codes

	// CountLength lists lengths of the character count field in four
	// Micro QR and three QR version size classes.
	CountLength [7]byte

	// EncodedLength returns the encoded data length in bits of a valid
	// string of the given length in bytes and runes.
	EncodedLength func(bytes, runes int) int

	// Valid reports whether the string is valid for the encoding mode.
	// It is called by Segment.IsValid and by the encoder.  If nil, the
	// string is validated using CutRune and Accepts.
	Valid func(string) bool

	// CutRune returns the first rune in the string and its width in
	// bytes.  If nil, utf8.DecodeRuneInString is used.  It should be
	// set if and only if the Mode requires non-UTF-8 rune decoding.
	CutRune func(string) (rune, int)

	// Accepts reports whether the encoding mode accepts the rune.
	// If nil, any rune is accepted.  It is called by Is.
	Accepts func(rune) bool

	// Transform returns a segment of another Mode with the string
	// transformed for encoding and a boolean indicating whether the
	// transform was successful.  The target Mode must have Transform
	// unset.  If nil, the original segment is used.  It is called by
	// Segment.Transform and by the encoder.
	Transform func(string) (Segment, bool)

	// Count returns the character count of the transformed string.
	// If nil, the length of the string in bytes is used.
	Count func(string) int

	// Encode3, Encode2 and Encode1 return the encoding of the bytes
	// and its length in bits.  The encoder calls a non-nil Encode{N}
	// repeatedly as long as N source bytes are available, in
	// descending order of N.  If all are nil, each byte is encoded as
	// 8 bits.  The encoder panics if not all bytes are consumed.
	Encode3 func([3]byte) (uint32, int)
	Encode2 func([2]byte) (uint32, int)
	Encode1 func(byte) (uint32, int)
}

const alphamask uint64 = 0x07fffffe_07ffec31 // SPACE $% *+ -./ [0-9] : [A-Z]

// Alphanumeric encoding table.  Used after validation.
// "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:"
var alpha = [64]byte{
	00, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, // 0x40
	25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 00, 00, 00, 00, 00, // 0x50
	36, 00, 00, 00, 37, 38, 00, 00, 00, 00, 39, 40, 00, 41, 42, 43, // 0x20
	00, 01, 02, 03, 04, 05, 06, 07, 010, 9, 44, 00, 00, 00, 00, 00, // 0x30
}

// Shift JIS / Shift JISx0213 table for ShiftJISKanji CutRune.
// Bit fields:
//
//	1 = valid 1st byte of multibyte character  0x81-0x9f, 0xe0-0xfc
//	2 = valid 2nd byte of multibyte character  0x40-0x7e, 0x80-0xfc
var sjistbl = [256]byte{
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x00
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x10
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x20
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x30
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 0x40
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 0x50
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 0x60
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, // 0x70
	2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // 0x80
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // 0x90
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 0xa0
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 0xb0
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 0xc0
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 0xd0
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // 0xe0
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, // 0xf0
}

func nothing(rune) bool { return false }

// IsKanji reports whether the Unicode rune r belongs to the QR Kanji
// subset of JIS X 0208.
func IsKanji(r rune) bool {
	// jis0208qr is an 8 KB table with one bit per character of
	// the Basic Multilingual Plane, with bits set for JIS X 0208
	// characters up to ku-ten 86-33.  It's less than half the
	// size of unicode.RangeTable and lookup is much faster.  This
	// could easily be done with 4 KB but lookup would be slower.
	x := uint32(r) >> 5
	return int(x) < len(jis0208qr) && jis0208qr[x]>>(r&0x1f)&1 != 0
}

var _stdmodes = []ModeEncoder{
	Numeric: {
		Name:          "numeric",
		Indicator:     1,
		CountLength:   [7]byte{3, 4, 5, 6, 10, 12, 14},
		EncodedLength: func(b, r int) int { return (10*b + 2) / 3 },
		Accepts:       func(r rune) bool { return uint32(r-'0') < 10 },
		Encode1: func(b byte) (uint32, int) {
			return uint32(b), 4
		},
		Encode2: func(b [2]byte) (uint32, int) {
			return uint32(b[0])*10 + uint32(b[1]) - '0'*11&0x7f, 7
		},
		Encode3: func(b [3]byte) (uint32, int) {
			return uint32(b[0])*100 + uint32(b[1])*10 +
				uint32(b[2]) + -'0'*111&0x3ff, 10
		},
	},
	Alphanumeric: {
		Name:          "alphanumeric",
		Indicator:     2,
		CountLength:   [7]byte{0, 3, 4, 5, 9, 11, 13},
		EncodedLength: func(b, r int) int { return (11*b + 1) / 2 },
		Accepts: func(r rune) bool {
			return alphamask>>(uint32(r)-' ')&1 != 0
		},
		Encode1: func(b byte) (uint32, int) {
			return uint32(alpha[b&0x3f]), 6
		},
		Encode2: func(b [2]byte) (uint32, int) {
			return uint32(alpha[b[0]&0x3f])*45 +
				uint32(alpha[b[1]&0x3f]), 11
		},
	},
	Byte: {
		Name:        "byte",
		Indicator:   4,
		CountLength: [7]byte{0, 0, 4, 5, 8, 16, 16},
	},
	Kanji: {
		Name:          "kanji",
		Indicator:     8,
		CountLength:   [7]byte{0, 0, 3, 4, 8, 10, 12},
		EncodedLength: func(b, r int) int { return r * 13 },
		Accepts:       IsKanji,
		Transform: func(s string) (Segment, bool) {
			t, err := japanese.ShiftJIS.NewEncoder().String(s)
			return Segment{t, ShiftJISKanji}, err == nil
		},
	},
	Latin1: {
		Name:          "latin-1",
		Indicator:     4,
		CountLength:   [7]byte{0, 0, 4, 5, 8, 16, 16},
		EncodedLength: func(b, r int) int { return r * 8 },
		Accepts:       func(r rune) bool { return uint32(r) < 0x100 },
		Transform: func(s string) (Segment, bool) {
			t, err := charmap.ISO8859_1.NewEncoder().String(s)
			return Segment{t, Byte}, err == nil
		},
	},
	ShiftJISKanji: {
		Name:          "shift-jis-kanji",
		Indicator:     8,
		CountLength:   [7]byte{0, 0, 3, 4, 8, 10, 12},
		EncodedLength: func(b, r int) int { return b >> 1 * 13 },
		Count:         func(s string) int { return len(s) >> 1 },
		CutRune: func(s string) (rune, int) {
			r, sz := rune(s[0]), 1
			if sjistbl[s[0]]&1 != 0 && len(s) > 1 &&
				sjistbl[s[1]]&2 != 0 {
				r, sz = r<<8|rune(s[1]), 2
			}
			return r, sz
		},
		Accepts: func(r rune) bool {
			const maxk = 0x1fff/0xc0<<8 | 0x1fff%0xc0 + 0xc140
			return uint32(r^0x8000) < maxk-0x8000+1
		},
		Encode2: func(b [2]byte) (uint32, int) {
			return uint32(b[0]&^0xc0)*0xc0 + uint32(b[1]) - 0x100,
				13
		},
	},
	FNC1Alpha: {
		Name:          "fnc1-alphanumeric",
		Indicator:     2,
		CountLength:   [7]byte{0, 3, 4, 5, 9, 11, 13},
		EncodedLength: func(b, r int) int { return (11*b + 1) / 2 },
		Accepts: func(r rune) bool {
			return alphamask>>(uint32(r)-' ')&1 != 0 || r == 0x1d
		},
		Transform: func(s string) (Segment, bool) {
			return Segment{strings.ReplaceAll(s, "\x1d", "%"),
				Alphanumeric}, true
		},
	},
	ECI: {
		Name:      "eci",
		Indicator: 7,
		Accepts:   nothing,
		Valid: func(s string) bool {
			ok := s != "" && len(s) == max(1, int(s[0]>>6))
			if ok && len(s) == 3 {
				ok = uint32(s[0]&^0xc0)<<16+uint32(s[1])<<8+
					uint32(s[2]) < 1e6
			}
			return ok
		},
	},
	StructAppend: {
		Name:      "structured-append",
		Indicator: 3,
		Accepts:   nothing,
		Valid: func(s string) bool {
			return len(s) == 2 && s[0]>>4 <= s[0]&0x0f
		},
	},
	FNC1First: {
		Name:      "fnc1-in-1st-position",
		Indicator: 5,
		Accepts:   nothing,
		Valid:     func(s string) bool { return s == "" },
	},
	FNC1Second: {
		Name:      "fnc1-in-2nd-position",
		Indicator: 9,
		Accepts:   nothing,
		Valid:     func(s string) bool { return len(s) == 1 },
	},
}

var (
	modep    atomic.Pointer[[]ModeEncoder] // modes
	modeLock sync.Mutex                    // write lock
)

func init() { modep.Store(&_stdmodes) }

func getMode(mode Mode) *ModeEncoder {
	if modes := *modep.Load(); mode >= 0 && int(mode) < len(modes) {
		return &modes[mode]
	}
	return nil
}

func (mode Mode) String() string {
	if m := getMode(mode); m != nil {
		return m.Name
	}
	return strconv.Itoa(int(mode))
}

// GetMode returns a copy of ModeEncoder for the mode.  It can be used
// to base the implementation of a new mode on an existing one.
func GetMode(mode Mode) *ModeEncoder {
	if m := getMode(mode); m != nil {
		mm := *m
		return &mm
	}
	return nil
}

// AddMode registers an encoding mode, returning its number on success
// or -1 on failure.  The number of modes is limited to 32768.
func AddMode(m *ModeEncoder) Mode {
	var mode Mode = -1
	modeLock.Lock()
	if modes := *modep.Load(); len(modes) < 0x8000 {
		mode = Mode(len(modes))
		modes = append(modes, *m)
		modep.Store(&modes)
	}
	modeLock.Unlock()
	return mode
}

// MinClass returns the lowest valid version size class for mode.
func (mode Mode) MinClass() int {
	if m := getMode(mode); m != nil {
		if ind := m.Indicator; ind&(ind-1) == 0 {
			return int(min(ind-1, ClassM3))
		}
	}
	return Class0
}

type (
	CutRuneFunc func(string) (rune, int) // Used in ModeEncoder.
	AcceptsFunc func(rune) bool          // Used in ModeEncoder.
)

// RuneFilter returns CutRune and Accepts functions for mode.
// If mode is invalid, RuneFilter returns nil and a function rejecting
// any rune.
func (mode Mode) RuneFilter() (CutRuneFunc, AcceptsFunc) {
	if m := getMode(mode); m != nil {
		return m.CutRune, m.Accepts
	}
	return nil, nothing
}

// length returns the length in bits of a valid string of the given
// length in bytes and runes encoded in mode at the given QR version
// size class, including the header.
func (m *ModeEncoder) length(bytes, runes, class int) int {
	n := min(class, 4) + int(m.CountLength[class])
	if f := m.EncodedLength; f != nil {
		n += f(bytes, runes)
	} else {
		n += bytes * 8
	}
	return n
}

// Length returns the length in bits of a valid string of the given
// length in bytes and runes encoded in mode at the given QR version
// size class, including the header.  Length returns 0 if and only if
// mode is invalid.
func (mode Mode) Length(bytes, runes int, class int) int {
	n := 0
	if m := getMode(mode); m != nil {
		n = m.length(bytes, runes, class)
	}
	return n
}

// Is reports whether r is encodable in mode.
func Is(r rune, mode Mode) bool {
	m := getMode(mode)
	return m != nil && (m.Accepts == nil || m.Accepts(r))
}

// A Segment describes a QR code segment.
type Segment struct {
	Text string // data to encode
	Mode Mode   // encoding mode
}

// SegmentError represents an invalid Segment.
type SegmentError Segment

func (e SegmentError) Error() string {
	if m := getMode(e.Mode); m != nil {
		return fmt.Sprintf("qr: non-%s string %#q", m.Name, e.Text)
	}
	return fmt.Sprintf("qr: invalid mode %d", e.Mode)
}

// ModeError represents an invalid Mode number or ModeEncoder.
type ModeError Mode

func (e ModeError) Error() string {
	return fmt.Sprintf("qr: invalid mode %s", Mode(e))
}

// CompatError represents an incompatibility between Mode and Version.
type CompatError struct {
	Mode
	Version
}

func (e CompatError) Error() string {
	return fmt.Sprintf("qr: mode %s not encodable in version %s",
		e.Mode, e.Version)
}

// isValid reports whether seg is encodable.
func (m *ModeEncoder) isValid(seg Segment) bool {
	if f := m.Valid; f != nil {
		return f(seg.Text)
	} else if is := m.Accepts; is != nil {
		if seg.Mode < 2 {
			for i := 0; i < len(seg.Text); i++ {
				if !is(rune(seg.Text[i])) {
					return false
				}
			}
		} else if cut := m.CutRune; cut != nil {
			for s := seg.Text; s != ""; {
				r, sz := cut(s)
				s = s[sz:]
				if !is(r) {
					return false
				}
			}
		} else {
			for _, r := range seg.Text {
				if !is(r) {
					return false
				}
			}
		}
	}
	return true
}

// IsValid reports whether seg is encodable.
func (seg Segment) IsValid() bool {
	if m := getMode(seg.Mode); m != nil {
		return m.isValid(seg)
	}
	return false
}

// EncodedLength returns the encoded length in bits of seg in the
// given QR version size class.  EncodedLength returns 0 if and only
// if mode is invalid.  The segment is not validated.
func (seg Segment) EncodedLength(class int) int {
	var rlen int
	m := getMode(seg.Mode)
	if m == nil {
		return 0
	} else if el := m.EncodedLength; el == nil || el(0, 0x100) == 0 {
	} else if cut := m.CutRune; cut != nil {
		for s := seg.Text; s != ""; rlen++ {
			_, sz := cut(s)
			s = s[sz:]
		}
	} else {
		rlen = utf8.RuneCountInString(seg.Text)
	}
	return m.length(len(seg.Text), rlen, class)
}

// transform transforms seg for encoding.  The transformed segment is
// not validated.  The encoder calls transform prior to encoding.
func (seg Segment) transform() (Segment, *ModeEncoder, error) {
	if m := getMode(seg.Mode); m == nil {
		return Segment{}, nil, ModeError(seg.Mode)
	} else if m.Transform == nil {
		return seg, m, nil
	} else if !m.isValid(seg) {
		return Segment{}, nil, SegmentError(seg)
	} else if ts, ok := m.Transform(seg.Text); !ok {
		return Segment{}, nil, SegmentError(seg)
	} else if m = getMode(ts.Mode); m == nil || m.Transform != nil {
		return Segment{}, nil, ModeError(seg.Mode)
	} else {
		return ts, m, nil
	}
}

// Transform transforms seg for encoding.  The transformed segment is
// not validated.  The encoder calls Transform prior to encoding.
func (seg Segment) Transform() (Segment, error) {
	if seg.Mode < Kanji || seg.Mode == ShiftJISKanji {
		return seg, nil
	}
	seg, _, err := seg.transform()
	return seg, err
}

// Encode writes seg encoded for the given QR version size class to b.
func (seg Segment) Encode(b *Bits, class int) error {
	// transform the string
	ts, m, err := seg.transform()
	if err != nil {
		return err
	} else if !m.isValid(ts) {
		return SegmentError(seg)
	}
	// write header
	s := ts.Text
	ind := uint32(m.Indicator)
	ilen := 4
	if class < 4 {
		ilen = class
		ii := ind>>1 - ind>>3
		if ind&(ind-1) != 0 || ii >= 1<<ilen {
			return CompatError{seg.Mode, Version(class) + M1}
		}
		ind = ii
	}
	b.Write(ind, ilen)
	w := len(s)
	if m.Count != nil {
		w = m.Count(s)
	}
	b.Write(uint32(w), int(m.CountLength[class]))
	// encode the string
	enc3, enc2, enc1 := m.Encode3, m.Encode2, m.Encode1
	if enc3 != nil || enc2 != nil || enc1 != nil {
		if enc3 != nil {
			for len(s) >= 3 {
				b.Write(enc3([3]byte{s[0], s[1], s[2]}))
				s = s[3:]
			}
		}
		if enc2 != nil {
			for len(s) >= 2 {
				b.Write(enc2([2]byte{s[0], s[1]}))
				s = s[2:]
			}
		}
		if enc1 != nil {
			for len(s) >= 1 {
				b.Write(enc1(s[0]))
				s = s[1:]
			}
		} else if s != "" {
			panic("qr: " + m.Name + " mode internal error")
		}
	} else if b.nbit&7 != 0 {
		for ; len(s) >= 4; s = s[4:] {
			v := uint32(s[0])<<24 | uint32(s[1])<<16 |
				uint32(s[2])<<8 | uint32(s[3])
			b.Write(v, 32)
		}
		if s != "" {
			var v uint32
			for i := 0; i < len(s); i++ {
				v = v<<8 | uint32(s[i])
			}
			b.Write(v, 8*len(s))
			s = ""
		}
	} else {
		b.b = append(b.b, s...)
		b.nbit += len(s) * 8
		s = ""
	}
	return nil
}

// A Level represents a QR error correction level.
// From least to most tolerant of errors, they are L, M, Q, H.
type Level int

const (
	L Level = iota
	M
	Q
	H
)

func (l Level) String() string {
	if L <= l && l <= H {
		return "LMQH"[l : l+1]
	}
	return strconv.Itoa(int(l))
}

// A Code is a square pixel grid.
type Code struct {
	Bitmap []byte // 1 is black, 0 is white
	Size   int    // number of pixels on a side
	Stride int    // number of bytes per row
}

func (c *Code) Black(x, y int) bool {
	return 0 <= x && x < c.Size && 0 <= y && y < c.Size &&
		c.Bitmap[y*c.Stride+x/8]&(1<<uint(7&^x)) != 0
}

// Penalty returns the penalty value for a QR code, or the negative
// evaluation score for a Micro QR code.  The value is used for
// choosing the mask.
func (c *Code) Penalty() int {
	siz, stride := c.Size, c.Stride
	bm := c.Bitmap

	if siz < 20 {
		// Micro QR code evaluation score: min(v,h)*16+max(v,h)
		//   v = number of dark modules in right side edge
		//   h = number of dark modules in lower side edge
		// v and h exclude timing modules.
		var v, h, shift = 0, 1, -siz & 7
		for i := stride*2 - 1; i < len(bm); i += stride {
			v -= int(bm[i] >> shift & 1)
		}
		for _, b := range bm[len(bm)-stride:] {
			for ; b != 0; b &= b - 1 {
				h--
			}
		}
		if h < v {
			h, v = v, h
		}
		return h<<4 + v
	}

	// Total penalty is the sum of penalties for runs and boxes
	// of same-colour pixels, finder patterns and colour balance.
	//
	//   - RunP: for non-overlapping runs of n pixels, n>=5 -> n-2
	//   - BoxP: for possibly overlapping 2x2 boxes -> 3
	//   - FindP: for possibly overlapping finder patterns -> 40
	//     The pattern is 010111010 with 000 on either side,
	//     or inverted; may extend into the quiet zone
	//   - BalP: for n% of black pixels -> 10*(celing(abs(n-50)/5)-1)
	//
	// https://www.nayuki.io/page/creating-a-qr-code-step-by-step
	const (
		MinRun    = 5             // RunP:  miniumum run length
		RunPDelta = -2            // RunP:  add to run length
		BoxPP     = 3             // BoxP:  points per box
		FindPP    = 40            // FindP: points per pattern
		BalPP     = 10            // BalP:  10 points
		BalPMul   = 20            //        for every 5% (1/20),
		BalPMax   = BalPMul/2 - 1 //        up to 9 times

		// last pixels are stored in a uint16, and when matching
		// against 12 bit finder patterns are shifted left 4 bits.
		pShift = 16 - 12
		// finder patterns:
		FindB = uint16(0b0000_1011101_0 << pShift) // quiet zone before
		FindA = uint16(0b0_1011101_0000 << pShift) // quiet zone after
		LoseB = ^FindB &^ (1<<pShift - 1)          // inverted FindB
		LoseA = ^FindA &^ (1<<pShift - 1)          // inverted FindA
	)

	p := 0   // total penalty
	bal := 0 // black pixels (shifted left 4)
	// horizontal runs: RunP, FindP, BoxP and count black pixels for BalP
	var line, prev []byte
	for len(bm) >= stride {
		prev, line, bm = line, bm[:stride], bm[stride:]
		r := 1                      // current run length for RunP
		pat := uint16(line[0] >> 3) // last 12 pixels for FindP, BoxP
		var pp uint16               // previous line pixels for BoxP
		if len(prev) != 0 {
			pp = uint16(prev[0] >> 3)
		}
		bal += int(pat) & (1 << pShift)
		// Scan rows from x=1.  BoxP is detected at the bottom right
		// pixel, RunP and FindP require even larger x.
		for x := 1; x < siz; x++ {
			pat = pat<<1 | uint16(line[x>>3])>>(7&^x)<<pShift
			if xx := x >> 3; xx < len(prev) {
				pp = pp<<1 | uint16(prev[xx])>>(7&^x)<<pShift
			}
			bal += int(pat) & (1 << pShift) // BalP count
			switch pat {
			case FindB, FindA, LoseB, LoseA:
				p += FindPP // FindP
			}
			if (pat-1<<pShift)&(2<<pShift) == 0 { // colour change
				if r >= MinRun {
					p += r + RunPDelta // RunP
				}
				r = 0
			} else if len(prev) != 0 && (pat^pp)&(3<<pShift) == 0 {
				p += BoxPP // BoxP
			}
			r++
		}
		// handle last run
		if r >= MinRun {
			p += r + RunPDelta // RunP
		}
		// handle FindB with 1 pixel in the right quiet zone;
		// also includes FindA with 4 pixels in the quiet zone
		if pat <<= 1; pat == FindB {
			p += 2 * FindPP // 2×FindP
		} else {
			// handle FindA with 1-4 pixels in quiet zone
			switch FindA {
			case pat, pat << 1, pat << 2, pat << 3:
				p += FindPP // FindP
			}
		}
	}

	// calculate BalP
	bal >>= pShift
	// Exact percentages get less penalty.  E.g., 40% and 60% get
	// 10 points like 41%, not 20 like 39%.  To round away from 50%,
	// fold bal into 0 <= n < c.Size²/2 and divide rounding down.
	// No need to handle 50% as c.Size is always odd.
	sq := c.Size * c.Size
	if bal > sq/2 {
		bal = sq - bal
	}
	p += (BalPMax - bal*BalPMul/sq) * BalPP

	// vertical runs: RunP, FindP
	bm = c.Bitmap
	for x := 0; x < siz; x++ {
		r := 1
		off, shift := x>>3, 7&^x
		pat := uint16(bm[off]) >> shift & 1 << pShift
		for off += stride; off < len(bm); off += stride {
			pat = pat<<1 | uint16(bm[off])>>shift&1<<pShift
			switch pat {
			case FindB, FindA, LoseB, LoseA:
				p += FindPP // FindP
			}
			if (pat-1<<pShift)&(2<<pShift) == 0 {
				if r >= MinRun {
					p += r + RunPDelta // RunP
				}
				r = 0
			}
			r++
		}
		if r >= MinRun {
			p += r + RunPDelta // RunP
		}
		if pat <<= 1; pat == FindB {
			p += 2 * FindPP // 2×FindP
		} else {
			switch FindA {
			case pat, pat << 1, pat << 2, pat << 3:
				p += FindPP // FindP
			}
		}
	}
	return p
}

// A Plan describes how to construct a QR code
// with a specific version and level.
type Plan struct {
	Version Version // QR code version
	Level   Level   // QR error correction Level

	DataBits int // number of data bits
	Size     int // number of pixels on a side

	Map     []byte    // pixel map: 0 is data or checksum, 1 is other
	Pattern [8][]byte // position and alignment boxes, timing, format, mask
}

// NewPlan returns a Plan for a QR code with the given version and level.
func NewPlan(version Version, level Level) (*Plan, error) {
	pp, err := makePlan(version, level)
	if err != nil {
		return nil, err
	}
	p := *pp
	siz := len(pp.Map)
	bitmap := make([]byte, cap(pp.Map))
	copy(bitmap, pp.Map[:cap(pp.Map)])
	p.Map, bitmap = bitmap[:siz], bitmap[siz:]
	for i := range p.Pattern {
		p.Pattern[i], bitmap = bitmap[:siz], bitmap[siz:]
		if len(bitmap) == 0 {
			break
		}
	}
	return &p, nil
}

// Pre-allocated Plans.  A Plan is created the first time a
// combination of version and level is used.  Each plan is 13 words
// plus a bitmap the size of 9 Code bitmaps (5 for Micro QR), from 567
// bytes for version 1 to 36 KB for version 40 and from 110 bytes for
// M1 to 255 bytes for M4.
var plans [M4 + 1][H + 1]struct {
	once sync.Once
	p    *Plan
}

// makePlan returns plans[version][level].
// If it doesn't exist, it is created.
func makePlan(version Version, level Level) (*Plan, error) {
	if version < MinVersion || version > M4 {
		return nil, ErrVersion
	}
	if level < L || level > H ||
		version >= M1 && version.dataBytes(level) == 0 {
		return nil, ErrLevel
	}
	p := &plans[version][level]
	if p.p == nil {
		p.once.Do(func() {
			pp := vplan(version, level)
			var fb []uint16
			if version < M1 {
				fb = ftab[level][:]
			} else {
				i := max(version<<1-(M1*2+1)+Version(level), 0)
				fb = mftab[i][:]
			}
			for mask, v := range fb {
				fplan(v, mask, pp)
				mplan(mask, pp)
			}
			p.p = pp
		})
	}
	return p.p, nil
}

func (b *Bits) padTo(t, n int) {
	b.nbit = min(b.nbit+t, n)
	for len(b.b)*8 < b.nbit {
		b.b = append(b.b, 0)
	}
	if len(b.b) < (n+7)>>3 {
		buf := b.b[len(b.b) : n>>3]
		b.b = b.b[:(n+7)>>3]
		b.b[len(b.b)-1] = 0
		for len(buf) >= 2 {
			buf[0], buf[1] = 0xec, 0x11
			buf = buf[2:]
		}
		if len(buf) > 0 {
			buf[0] = 0xec
		}
	}
	b.nbit = len(b.b) * 8
}

// PadTo adds up to t terminator bits to b and pads it to n bit.
func (b *Bits) PadTo(t, n int) {
	b.growTo(n)
	b.padTo(t, n)
}

// AddCheckBytes adds terminator, padding and checksum to b for the
// given QR version and level.
func (b *Bits) AddCheckBytes(v Version, l Level) {
	nb := v.DataBits(l)
	if b.nbit > nb {
		panic("qr: too much data")
	}
	vt := &vtab[v]
	b.growTo(vt.bytes)
	nt := 4
	if v >= M1 {
		nt = int(v*2 - M1*2 + 3)
	}
	b.padTo(nt, nb)
	nd := (nb + 4) >> 3

	dat := b.Bytes()
	lev := vt.level[l]
	db := nd / lev.nblock
	normal := (db+1)*lev.nblock - nd
	rs := gf256.NewRSEncoder(Field, lev.check)
	for i := 0; i < lev.nblock; i++ {
		if i == normal {
			db++
		}
		rs.ECC(dat[:db], b.Add(lev.check))
		dat = dat[db:]
	}

	if len(b.Bytes()) != vt.bytes {
		panic("qr: internal error")
	}
	if nb&4 != 0 {
		chk := b.b[nb>>3:]
		for i := range chk[:len(chk)-1] {
			chk[i] |= chk[i+1] >> 4
			chk[i+1] <<= 4
		}
	}
}

// interleave interleaves nblock blocks from src to dst, which must be
// of equal length.
func interleave(dst, src []byte, nblock int) {
	db := len(src) / nblock
	extra := dst[db*nblock:]
	dst = dst[:db*nblock]
	normal := nblock - len(extra)
	for i := 0; i < nblock; i++ {
		for j, v := range src[:db] {
			dst[j*nblock+i] = v
		}
		src = src[db:]
		if i >= normal {
			extra[i-normal] = src[0]
			src = src[1:]
		}
	}
}

// Permute returns a BitStream reading data and checksum bits in b
// with blocks interleaved for the given QR code version and level.
// The BitStream may use the same underlying buffer.
func (b *Bits) Permute(v Version, l Level) BitStream {
	vt := &vtab[v]
	src := b.Bytes()
	if len(src) != vt.bytes {
		panic("qr: wrong data length")
	}
	dst := src
	if nblock := vt.level[l].nblock; nblock != 1 {
		if cap(src) < len(src)*2 {
			dst = make([]byte, vt.bytes)
		} else {
			dst = src[len(src) : len(src)*2]
		}
		nd := v.dataBytes(l)
		interleave(dst[:nd], src[:nd], nblock)
		interleave(dst[nd:], src[nd:], nblock)
	}
	return NewBitStream(dst)
}

// BitStream reads bits from the underlying buffer.
type BitStream struct {
	b   []byte
	pos int
}

// NewBitStream returns a BitStream reading from b.
func NewBitStream(b []byte) BitStream { return BitStream{b: b} }

// Bytes returns the data underlying s.
func (s *BitStream) Bytes() []byte { return s.b }

// Next returns the next bit from s as 0 or 1.
// Past end of buffer Next returns 0.
func (s *BitStream) Next() byte {
	var b byte
	if i := s.pos >> 3; i < len(s.b) {
		b = s.b[i] >> (7 &^ s.pos) & 1
		s.pos++
	}
	return b
}

// Serialise writes bits from s to the bitmap in zigzag scan order.
func (p *Plan) Serialise(s BitStream, bitmap []byte) {
	siz := p.Size
	stride := (siz + 7) >> 3
	pmap := p.Map
	for x := siz - 2; x >= 0; {
		lx, lb := x>>3, byte(0x80)>>(x&7)
		rxOff, rb := int(lb&1), byte(0x80)>>((x+1)&7)
		for off := (siz-1)*stride + lx; off >= 0; off -= stride {
			if pmap[off+rxOff]&rb == 0 && s.Next() != 0 {
				bitmap[off+rxOff] ^= rb
			}
			if pmap[off]&lb == 0 && s.Next() != 0 {
				bitmap[off] ^= lb
			}
		}
		x -= 2
		if x < 0 {
			return
		} else if x == 5 && siz > 20 { // vertical timing strip
			x--
		}
		lx, lb = x>>3, byte(0x80)>>(x&7)
		rxOff, rb = int(lb&1), byte(0x80)>>((x+1)&7)
		for off := lx; off < len(pmap); off += stride {
			if pmap[off+rxOff]&rb == 0 && s.Next() != 0 {
				bitmap[off+rxOff] ^= rb
			}
			if pmap[off]&lb == 0 && s.Next() != 0 {
				bitmap[off] ^= lb
			}
		}
		x -= 2
	}
}

// Encoder encodes a QR code.
type Encoder struct {
	p *Plan
	b *Bits
}

func newEncoder(p *Plan) *Encoder {
	return &Encoder{p: p, b: NewBits(p.Version, p.Level)}
}

// NewEncoder returns an Encoder for the given version and level.
func NewEncoder(version Version, level Level) (*Encoder, error) {
	p, err := makePlan(version, level)
	if err != nil {
		return nil, err
	}
	return newEncoder(p), nil
}

// Write adds text to e.
func (e *Encoder) Write(text ...Segment) error {
	class := e.p.Version.SizeClass()
	for _, t := range text {
		if err := t.Encode(e.b, class); err != nil {
			return err
		}
	}
	return nil
}

// xor xors a and b into dst.  a and b may not be shorter than dst.
// dst and a or b should not overlap unless they are the same slice.
func xor(dst, a, b []byte) {
	a = a[:len(dst)]
	b = b[:len(dst)]
	for i := range dst {
		dst[i] = a[i] ^ b[i]
	}
}

func (e *Encoder) Reset() { e.b.Reset() }

// Code returns a QR code containing data written to e.
func (e *Encoder) Code() (*Code, error) {
	if e.b.Bits() > e.p.DataBits {
		return nil, fmt.Errorf("cannot encode %d bits into %d-bit code",
			e.b.Bits(), e.p.DataBits)
	}
	e.b.AddCheckBytes(e.p.Version, e.p.Level)
	bits := e.b.Permute(e.p.Version, e.p.Level)
	// Now we have the checksum bytes and the data bytes.
	// Construct the bitmap consisting of data and checksum bits.
	siz, stride := e.p.Size, (e.p.Size+7)>>3
	data := make([]byte, siz*stride)
	e.p.Serialise(bits, data)

	// Apply masks to the bitmap to construct the actual codes.
	// Choose the code with the smallest penalty.
	c := &Code{Size: siz, Stride: stride, Bitmap: make([]byte, len(data))}
	best := make([]byte, len(data)) // best bitmap so far
	pen := 1 << 30                  // largest penalty is < 1<<20
	for _, v := range e.p.Pattern {
		if len(v) == 0 {
			break
		}
		// set bitmap to data bits xor plan bits
		xor(c.Bitmap, data, v)
		if p := c.Penalty(); p < pen {
			best, pen, c.Bitmap = c.Bitmap, p, best
		}
	}
	c.Bitmap = best
	return c, nil
}

// Encode is a wrapper around Write and Code.
func (e *Encoder) Encode(text ...Segment) (*Code, error) {
	if err := e.Write(text...); err != nil {
		return nil, err
	}
	return e.Code()
}

func (p *Plan) Encode(text ...Segment) (*Code, error) {
	return newEncoder(p).Encode(text...)
}

// Encode encodes text using an Encoder with the given version and level.
func Encode(version Version, level Level, text ...Segment) (*Code, error) {
	e, err := NewEncoder(version, level)
	if err != nil {
		return nil, err
	}
	return e.Encode(text...)
}

// A version describes metadata associated with a version.
type version struct {
	apos    int
	astride int
	bytes   int
	pattern int
	level   [4]level
}

type level struct {
	nblock int
	check  int
}

func set16(b []byte, bits uint16) {
	_ = b[1]
	b[0] |= byte(bits >> 8)
	b[1] |= byte(bits)
}

// vplan creates a Plan for the given version.
func vplan(v Version, l Level) *Plan {
	info := &vtab[v]
	p := &Plan{
		Version:  v,
		Level:    l,
		DataBits: v.DataBits(l),
	}
	var siz int
	masks := 8
	µ := v >= M1
	if !µ {
		siz = int(v*4 + 17)
	} else {
		siz = int(v*2 - M1*2 + 11)
		masks = 4
	}
	stride := (siz + 7) >> 3
	p.Size = siz
	bitmap := make([]byte, stride*siz*(masks+1))
	p.Map, bitmap = bitmap[:stride*siz], bitmap[stride*siz:]
	p.Pattern[0] = bitmap

	// Timing markers (overwritten by boxes).
	// Vertical.  Mask ends of rows.
	tdot := byte(0x02)
	if µ {
		tdot = 0x80
	}
	mpat := uint16(0xffff) >> (p.Size & 7) & (0xff00 | uint16(tdot))
	for n := stride - 1; n+1 < len(p.Map); n += stride {
		set16(p.Map[n:], mpat)
		n += stride
		set16(p.Map[n:], mpat)
		bitmap[n+1] = tdot
	}
	p.Map[len(p.Map)-1] = byte(mpat >> 8)
	// Horizontal.
	if µ {
		p.Map[1] = 0xff
		p.Map[2] = 0xff
		set16(bitmap[1:], 0xaaaa&^(0xffff>>(siz-8)))
	} else {
		for n := stride*6 + 1; n < stride*7-1; n++ {
			p.Map[n] = 0xff
			bitmap[n] = 0xaa
		}
	}

	// Position boxes.
	// Mask 9x9 pixels on top left, 8x9 on top right, 9x8 on bottom left.
	off := stride - 2
	shift := 6 &^ siz
	lpat := uint64(0xfe82bababa82fe)
	mpat = 0x1fe << shift
	for i, s, e := 0, 0, len(p.Map)-stride; ; i++ {
		set16(p.Map[s:], 0xff80) // top left
		if !µ {
			set16(p.Map[s+off:], mpat) // top right
			if i == 8 {
				break
			}
			set16(p.Map[e:], 0xff80) // bottom left
			bitmap[e] = byte(lpat)
			set16(bitmap[s+off:], uint16(lpat&0xff)<<shift)
			e -= stride
		} else if i == 8 {
			goto End
		}
		bitmap[s] = byte(lpat)
		lpat >>= 8
		s += stride
	}

	// Alignment boxes.
	for x := info.apos; ; x += info.astride {
		for y := info.apos; y < siz; y += info.astride {
			alignBox(p, x, y)
		}
		if x >= siz-12 {
			break
		}
		alignBox(p, x, 4)
		alignBox(p, 4, x)
	}

	// Version pattern.
	if v := info.pattern; v != 0 {
		// vpat: 3x6 pixels at (siz-11, 0)
		// hpat: 6x3 pixels at (0, siz-11)
		off := (siz - 11) / 8
		shift := (siz - 11) & 7
		mpat = 0xe000 >> shift
		var hpat uint32
		for x := 0; x < 6; x++ {
			vpat := uint32(v&7) * 0x421 & 0x1041
			hpat = hpat<<1 | vpat
			vpat = vpat * 0x8102 & 0xe000 >> shift
			v >>= 3
			set16(p.Map[off:], mpat)
			set16(bitmap[off:], uint16(vpat))
			off += stride
		}
		off = (siz - 11) * stride
		for i := 0; i < 3; i++ {
			p.Map[off] = 0xfe
			bitmap[off] |= byte(hpat << 2)
			hpat >>= 6
			off += stride
		}
	}

	// One lonely black pixel
	bitmap[(siz-8)*stride+1] = 0x80

End:
	sz := len(p.Map)
	for n := sz; n < len(bitmap); {
		n += copy(bitmap[n:], bitmap[:n])
	}
	for i := range p.Pattern {
		p.Pattern[i], bitmap = bitmap[:sz], bitmap[sz:]
		if len(bitmap) == 0 {
			break
		}
	}
	return p
}

// fplan sets the format bits
func fplan(fb uint16, mask int, p *Plan) {
	// Format pixels.
	b := p.Pattern[mask]
	siz := p.Size
	stride := (siz + 7) >> 3
	off := 1
	µ := siz < 20
	if µ {
		off += stride
	}
	for i, v := 0, fb; i < 15; i++ {
		switch i {
		case 6:
			off = stride*7 + 1
		case 8:
			if µ {
				b[8*stride] |= byte(fb >> 8)
				return
			}
			off = stride*(siz-7) + 1
		}
		b[off] |= byte(v << 7)
		v >>= 1
		off += stride
	}
	off = 8 * stride
	hi := byte(fb >> 8)
	b[off] |= hi<<1 | hi&0x01
	fb = fb & 0xff << (-siz & 7)
	set16(b[off+stride-2:], fb)
}

// Mask patterns:
//
//	0: ▄▀▄▀▄▀▄▀▄▀▄▀  1: ▄▄▄▄▄▄▄▄▄▄▄▄  2:  ██ ██ ██ ██  3: ▄█▀▄█▀▄█▀▄█▀
//	   ▄▀▄▀▄▀▄▀▄▀▄▀     ▄▄▄▄▄▄▄▄▄▄▄▄      ██ ██ ██ ██     ▀▄█▀▄█▀▄█▀▄█
//	   ▄▀▄▀▄▀▄▀▄▀▄▀     ▄▄▄▄▄▄▄▄▄▄▄▄      ██ ██ ██ ██     █▀▄█▀▄█▀▄█▀▄
//	   ▄▀▄▀▄▀▄▀▄▀▄▀     ▄▄▄▄▄▄▄▄▄▄▄▄      ██ ██ ██ ██     ▄█▀▄█▀▄█▀▄█▀
//	   ▄▀▄▀▄▀▄▀▄▀▄▀     ▄▄▄▄▄▄▄▄▄▄▄▄      ██ ██ ██ ██     ▀▄█▀▄█▀▄█▀▄█
//	   ▄▀▄▀▄▀▄▀▄▀▄▀     ▄▄▄▄▄▄▄▄▄▄▄▄      ██ ██ ██ ██     █▀▄█▀▄█▀▄█▀▄
//
//	4:    ███   ███  5:  ▄▄▄▄▄ ▄▄▄▄▄  6:    ▄▄▄   ▄▄▄  7: ▄█▄▀ ▀▄█▄▀ ▀
//	   ███   ███         █▀▄▀█ █▀▄▀█      ▄▀▄ █ ▄▀▄ █     ▄▀█▀▄ ▄▀█▀▄
//	      ███   ███      ██▄██ ██▄██      █▄▄▀  █▄▄▀      ▄  ▀██▄  ▀██
//	   ███   ███         ▄▄▄▄▄ ▄▄▄▄▄        ▄▄▄   ▄▄▄     ▄█▄▀ ▀▄█▄▀ ▀
//	      ███   ███      █▀▄▀█ █▀▄▀█      ▄▀▄ █ ▄▀▄ █     ▄▀█▀▄ ▄▀█▀▄
//	   ███   ███         ██▄██ ██▄██      █▄▄▀  █▄▄▀      ▄  ▀██▄  ▀██
var maskPat = [8][]uint16{
	{05252, 02525},
	{07777, 00000},
	{04444},
	{04444, 01111, 02222},
	{07070, 07070, 00707, 00707},
	{07777, 04040, 04444, 05252, 04444, 04040},
	{07777, 07070, 06666, 05252, 05555, 04343},
	{05252, 00707, 04343, 02525, 07070, 03434},
}

// mplan edits a version+level-only Plan to add the mask.
func mplan(mask int, p *Plan) {
	stride := (p.Size + 7) >> 3
	var mpbuf [(MaxVersion*4 + 17 + 7) / 8 * 6]byte // 136 byte array
	mm := mask
	b := p.Pattern[mask]
	m := p.Map[:len(b)]
	if p.Size < 20 {
		mm = [4]int{1, 4, 6, 7}[mask]
	}
	mpx := maskPat[mm] // mask patterns
	// create a pattern of 1-6 rows of 2-23 bytes (3-136 bytes)
	for i, v := range mpx {
		pr := mpbuf[i*stride:]
		_ = pr[2]
		pr[0], pr[1], pr[2] = byte(v>>4), byte(v>>2), byte(v)
		pr = pr[:stride]
		for n := 3; n < len(pr); n += copy(pr[n:], pr[:n]) {
		}
	}
	mp := mpbuf[:len(mpx)*stride] // mask pattern
	// apply mask pattern
	for len(b) != 0 {
		ml := min(len(b), len(mp))
		bb, mm := b[:ml], m[:ml]
		b, m = b[ml:], m[ml:]
		for i, v := range mp[:ml] {
			bb[i] |= v &^ mm[i]
		}
	}
}

// alignBox draw an alignment (small) box at upper left x, y.
func alignBox(p *Plan, x, y int) {
	stride := (p.Size + 7) >> 3
	mpat := uint32(0xf800) >> (x & 7)
	bpat := uint32(0xf8a8f800) >> (x & 7)
	for off := y*stride + x>>3; bpat > mpat; off += stride {
		set16(p.Map[off:], uint16(mpat))
		set16(p.Pattern[0][off:], uint16(bpat&mpat))
		bpat >>= 4
	}
}

/*
func printQR(c Code) {
	for y := -4; y < c.Size+4; y += 2 {
		for x := -4; x < c.Size+4; x++ {
			n := 0
			if c.Black(x, y) {
				n = 2
			}
			if c.Black(x, y+1) {
				n++
			}
			print([4]string{"█", "▀", "▄", " "}[n&3])
		}
		println()
	}
}
*/
