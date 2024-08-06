// Copyright 2024 Vadim Vygonets.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

/*
Package split splits strings into QR code segments.
*/
package split // import "github.com/unixdj/qr/split"

import (
	"errors"
	"unicode/utf8"

	"github.com/unixdj/qr/coding"
)

// QR error correction levels.
const (
	L = coding.L // 20% redundant
	M = coding.M // 38% redundant
	Q = coding.Q // 55% redundant
	H = coding.H // 65% redundant
)

// Predefined Charsets.
const (
	// UTF8 is the UTF-8 Charset.
	UTF8 = UTF8Charset(Byte)

	// UTF8AsLatin1 is a Charset for UTF-8 input encoding byte mode
	// segments as ISO 8859-1.
	UTF8AsLatin1 = UTF8Charset(Latin1)

	// ShiftJIS is the Shift JIS Charset.
	ShiftJIS = ASCIICompatCharset(ShiftJISKanji)

	// ASCIICompat is a Charset for ASCII-compatible eight bit
	// character encodings.
	ASCIICompat = ASCIICompatCharset(Disabled)
)

/*
Predefined modes.

A Mode encodes QR code segments.  Modes are used in Segment.Mode and
ModeList.  Charsets using combinations of predefined Modes are
documented under UTF8.  Mode encoders can be added using
coding.AddMode.

	Mode           QR segment mode       Character encoding
	Numeric        numeric               any ASCII-compatible encoding
	Alphanumeric   alphanumeric          any ASCII-compatible encoding
	Byte           byte                  any data
	Kanji          kanji                 UTF-8
	Latin1         byte                  UTF-8 input encoded as ISO 8859-1
	ShiftJISKanji  kanji                 Shift JIS, Shift JISx0213
	ECI            eci                   N/A
	StructAppend   structured append     N/A

Numeric and Alphanumeric accept the respective QR encoding modes'
character sets.  Byte encodes any string as is.  Latin1 accepts UTF-8
characters up to U+00FF, encoding them as Latin-1.

Kanji accepts UTF-8 code points corresponding to JIS X 0208 characters
encodable in QR Kanji mode, up to ku-ten 86-33.  ShiftJISKanji accepts
any multibyte character up to 0xeb 0xbf, which additionally includes
JIS X 0213 characters up to 1-86-33.  Its CutRune returns a valid
multibyte Shift JISx0213 character as a big endian 16 bit value, and
decodes anything else, including invalid encoding, as a single byte.

ECI and StructAppend encode a raw segment.  They are not usable with
Charset.  SetECI, ShouldSetECI, MustSetECI and Text create ECI Mode
Segments.  SplitMulti generates StructAppend Mode Segments.

Disabled is used in ModeList.
*/
const (
	Numeric       = coding.Numeric
	Alphanumeric  = coding.Alphanumeric
	Byte          = coding.Byte
	Kanji         = coding.Kanji
	Latin1        = coding.Latin1
	ShiftJISKanji = coding.ShiftJISKanji
	ECI           = coding.ECI
	StructAppend  = coding.StructAppend
	Disabled      = coding.Mode(-1)
)

// A Data is data encodable in a QR code.
//
// Data is implemented by String, Segment, List and Null.
type Data interface {
	// MinLength returns the minimum theoretically possible encoded
	// length for the data.  The returned value doesn't have to be
	// accurate, but it MUST NOT be greater than any value returned
	// by the Splitter's Split method.
	MinLength() int

	// Splitter returns a Splitter for the data.
	Splitter() (Splitter, error)
}

/*
A Splitter splits data into QR code segments.

Split splits data into segments for the given QR version size class
and returns the Result and its encoded length in bits.  Split may be
called more than once.  The Result may use the same underlying data
structure.

SplitTo returns the Result of up to lim bits and its length for the
beginning of the data, and a Splitter for the rest.  If no data fits
in lim bits, the Result must be nil.  If all data fits, the returned
Splitter must be nil.  After SplitTo the original Splitter is no
longer usable.
*/
type Splitter interface {
	Split(class int) (Result, int)
	SplitTo(lim, class int) (Result, int, Splitter)
}

// A Result is a result of a split returned by Splitter methods.
type Result interface {
	// Len returns the number of segments.
	Len() int

	// Append appends the segments to the slice.
	Append([]coding.Segment) []coding.Segment
}

var (
	ErrLongHeader   = errors.New("qr: header too long")
	ErrLongText     = errors.New("qr: text too long")
	ErrNotEncodable = errors.New("qr: text not encodable in given modes")
	ErrECI          = errors.New("qr: invalid eci number")
)

var (
	sizeClass = [3]struct{ min, max coding.Version }{
		{1, 9}, {10, 26}, {27, 40},
	}

	sizeLimit = [4][3]int{
		L: {
			sizeClass[0].max.DataBytes(L) * 8,
			sizeClass[1].max.DataBytes(L) * 8,
			sizeClass[2].max.DataBytes(L) * 8,
		},
		M: {
			sizeClass[0].max.DataBytes(M) * 8,
			sizeClass[1].max.DataBytes(M) * 8,
			sizeClass[2].max.DataBytes(M) * 8,
		},
		Q: {
			sizeClass[0].max.DataBytes(Q) * 8,
			sizeClass[1].max.DataBytes(Q) * 8,
			sizeClass[2].max.DataBytes(Q) * 8,
		},
		H: {
			sizeClass[0].max.DataBytes(H) * 8,
			sizeClass[1].max.DataBytes(H) * 8,
			sizeClass[2].max.DataBytes(H) * 8,
		},
	}
)

/*
Split returns segments and minimum QR code version for data at the
given error correction level.

Using String{Text: text} as data returns parameters for encoding bare
UTF-8 text.  The Text function constructs Data for common scenarios.
*/
func Split(data Data, level coding.Level) ([]coding.Segment, coding.Version, error) {
	if level < L || level > H {
		return nil, 0, coding.ErrLevel
	}
	lim := sizeLimit[level]
	// Estimate minimum QR version size class.  This is done in a
	// very crude manner, as it's likely to be completely off anyway.
	bits := data.MinLength()
	class := 0
	for lim[class] < bits {
		if class++; class == 3 {
			return nil, 0, ErrLongText
		}
	}
	// Split Strings into spans.
	sp, err := data.Splitter()
	if err != nil {
		return nil, 0, err
	}

	// Split data into segments for the size class.
	r, bits := sp.Split(class)
	// If data is too big for the size class, increment class
	// and resplit.  bits will change, hence the loop.
	for lim[class] < bits {
		for class++; class < 3 && lim[class] < bits; class++ {
		}
		if class == 3 {
			return nil, 0, ErrLongText
		}
		r, bits = sp.Split(class)
	}

	// Find version in the size class.
	v := sizeClass[class].min
	for max := sizeClass[class].max; v < max; {
		if mid := (v + max) / 2; mid.DataBytes(level)*8 < bits {
			v = mid + 1
		} else {
			max = mid
		}
	}

	// Get the segments.
	cs := r.Append(make([]coding.Segment, 0, r.Len()))
	return cs, v, nil
}

// parity returns the Structured Append Parity Data (xor of all bytes)
// for a text mode segment, or 0 for any other segment.
func parity(seg coding.Segment) byte {
	var par byte
	if seg.Mode < ECI {
		for i := 0; i < len(seg.Text); i++ {
			par ^= seg.Text[i]
		}
	}
	return par
}

// SplitMulti returns segments for data split across up to 16 QR codes
// ("Structured Append symbols") of the given version and error
// correction level, with header at the beginning of each code.
// header may be nil.
func SplitMulti(header, data Data, ver coding.Version, level coding.Level) ([][]coding.Segment, error) {
	if ver < coding.MinVersion || ver > coding.MaxVersion {
		return nil, coding.ErrVersion
	}
	if level < L || level > H {
		return nil, coding.ErrLevel
	}
	const (
		maxCodes = 16      // maximum Structured Append Symbols
		sabits   = 4 + 2*8 // Structured Append Header bit size
	)

	// prepare Structured Append segment and header
	if header == nil {
		header = Null{}
	}
	hs, err := header.Splitter()
	if err != nil {
		return nil, err
	}
	class := ver.SizeClass()
	hr, hbits := hs.Split(class)

	// calculate data size per code
	dsize := ver.DataBytes(level)*8 - sabits - hbits
	if dsize < 4 {
		return nil, ErrLongHeader
	} else if data.MinLength() > dsize*maxCodes {
		return nil, ErrLongText
	}

	// split data
	sp, err := data.Splitter()
	if err != nil {
		return nil, err
	}
	parts := make([]Result, 0, maxCodes)
	for sp != nil {
		if len(parts) == maxCodes {
			return nil, ErrLongText
		}
		var r Result
		if r, _, sp = sp.SplitTo(dsize, class); r == nil {
			return nil, ErrLongHeader
		}
		parts = append(parts, r)
	}

	// allocate segments, copy structured append segment and header
	segs := make([][]coding.Segment, len(parts))
	hsegs := 1 + hr.Len()
	segs[0] = hr.Append(make([]coding.Segment, 1, hsegs+parts[0].Len()))
	segs[0][0].Mode = StructAppend
	for i := 1; i < len(segs); i++ {
		segs[i] = make([]coding.Segment, hsegs, hsegs+parts[i].Len())
		copy(segs[i], segs[0])
	}
	var par byte // parity
	// if odd number of codes, calculate header parity
	if len(segs)&1 != 0 {
		for j, v := 1, segs[0]; j < len(v); j++ {
			if v[j], err = v[j].Transform(); err != nil {
				return nil, err
			}
			par ^= parity(v[j])
		}
	}

	// copy data segments, transform (for Kanji), calculate parity
	for i := range segs {
		segs[i] = parts[i].Append(segs[i])
		for j, v := hsegs, segs[i]; j < len(v); j++ {
			if v[j], err = v[j].Transform(); err != nil {
				return nil, err
			}
			par ^= parity(v[j])
		}
	}

	// set Structured Append Data for all parts
	sad := make([]byte, len(segs)*2)
	num := byte(len(segs) - 1)
	for i := 0; i < len(sad); i += 2 {
		sad[i], sad[i+1] = byte(i)<<3|num, par
	}
	sas := string(sad)
	for i := range segs {
		segs[i][0].Text, sas = sas[:2], sas[2:]
	}
	return segs, nil
}

// Null represents no data.
// It implements Data, Splitter and Result.
type Null struct{}

func (Null) MinLength() int                           { return 0 }
func (Null) Splitter() (Splitter, error)              { return Null{}, nil }
func (Null) Split(int) (Result, int)                  { return Null{}, 0 }
func (Null) SplitTo(int, int) (Result, int, Splitter) { return Null{}, 0, nil }

func (Null) Len() int                                   { return 0 }
func (Null) Append(a []coding.Segment) []coding.Segment { return a }

// List is a slice of Data that implements Data.
type List []Data

func (l List) MinLength() int {
	var n int
	for i := range l {
		n += l[i].MinLength()
	}
	return n
}

func (l List) Splitter() (Splitter, error) {
	sl := make(splitList, len(l))
	for i := range l {
		var err error
		if sl[i].Splitter, err = l[i].Splitter(); err != nil {
			return nil, err
		}
	}
	return sl, nil
}

// splitList is a Splitter containing Splitters and a Result
// containing Results.
type splitList []struct {
	Splitter
	Result
}

func (l splitList) Split(class int) (Result, int) {
	var n, nn int
	for i := range l {
		l[i].Result, nn = l[i].Split(class)
		n += nn
	}
	return l, n
}

func (l splitList) SplitTo(lim, class int) (Result, int, Splitter) {
	var n, nn int
	for i := range l {
		l[i].Result, nn, l[i].Splitter = l[i].SplitTo(lim-n, class)
		n += nn
		if l[i].Result == nil {
			if i == 0 {
				return nil, 0, l
			}
			return l[:i], n, l[i:]
		}
		if l[i].Splitter != nil {
			// Done.  Result and Splitter overlap at l[i].
			// Can't have it, as next call will overwrite
			// l[i].Result.  Handle, rather optimising Splitter.
			if i == len(l)-1 {
				return l, n, l[i].Splitter
			} else if i == 0 {
				return l[0].Result, n, l
			}
			return splitList{{Result: l[:i]}, l[i]}, n, l[i:]
		}
	}
	return l, n, nil
}

func (l splitList) Len() int {
	var n int
	for i := range l {
		n += l[i].Len()
	}
	return n
}

func (l splitList) Append(a []coding.Segment) []coding.Segment {
	for i := range l {
		a = l[i].Append(a)
	}
	return a
}

// Segment describes a QR code segment.
// It implements Data, Splitter and Result.
type Segment struct {
	Text string
	Mode coding.Mode
}

func (seg Segment) Splitter() (Splitter, error) { return seg, nil }
func (seg Segment) Len() int                    { return 1 }

func (seg Segment) MinLength() int {
	return seg.Mode.Length(len(seg.Text), 0, 0)
}

// Split returns seg and its encoded length at the given QR version
// size class.
func (seg Segment) Split(class int) (Result, int) {
	return seg, coding.Segment(seg).EncodedLength(class)
}

// SplitTo returns seg, its encoded length and nil if the encoding
// fits in lim bits, otherwise it returns (nil, 0, seg).
func (seg Segment) SplitTo(lim, class int) (Result, int, Splitter) {
	if siz := coding.Segment(seg).EncodedLength(class); siz <= lim {
		return seg, siz, nil
	}
	return nil, 0, seg
}

// Append appends seg to a.
func (seg Segment) Append(a []coding.Segment) []coding.Segment {
	return append(a, coding.Segment(seg))
}

// String describes a string to encode.  It implements Data.
//
// Text is split into numeric, alphanumeric, byte and kanji mode
// segments to minimise the encoded length, as determined by Charset.
// If Charset is nil, it defaults to UTF8.
type String struct {
	Text    string
	Charset Charset
}

func (s String) MinLength() int {
	if s.Text == "" {
		return 0
	}
	if s.Charset == nil {
		s.Charset = UTF8
	}
	return s.Charset.MinLength(len(s.Text))
}

/*
sSplitter and its component types.

String.Splitter determines modes in which each rune in the string is
encodable and creates a slice of spans, each span describing a
substring of runes encodable in the same modes.  To avoid multiple
allocations, the span structure contains an array of segments for the
modes.

sSplitter.Split creates a linked list of segments representing an
optimal split of the data.  A segment contains its mode, length in
bytes and runes, total encoded length in bits of the string from this
segment to the end, and a link to the next segment.

The split is calculated by walking the spans backwards, though the
algorithm works in either direction, as shown below.  For each span n,
for each mode m, a segment (n,m) is created representing an optimal
split for the string from span n to the end, starting with mode m.  A
segment with the shortest total encoding in span n is termed (n,best).

The segment (n,m) is created thusly.  Up to two segments are created,
one linking to (n+1,best) and another merged with (n+1,m), if such
exists.  Either may be optimised out.  The encoded length of each
segment is calculated, and the total encoded length of the next
segment is added to it.  The segment with the smaller total encoded
length is chosen, preferring the merged segment.

When the beginning of the span slice is reached, the segment (0,best)
describes an optimal split for the whole string.

sSplitter.SplitTo walks the spans forward in the same manner until the
encoding exceeds the size limit.  It then splits the last span in two
at the rightmost rune boundary at which the encoding fits into the
specified size, adding the initial part to the previous span.  After
that it reverses the linked list.
*/
type (
	// segment describes a segment encoded in a certain mode.
	segment struct {
		mode    coding.Mode // encoding mode
		segdata             // lengths and pointer to next
	}

	// segdata is the mutable portion of segment.
	segdata struct {
		next *segment // link to next segment in the chain
		len  uint32   // length of string in bytes
		rlen uint32   // length of string in runes
		bits uint32   // encoded size of all segments in the chain
	}

	// span describes a span of bytes encodable in the same modes.
	span struct {
		len  uint32     // length of string in bytes
		rlen uint32     // length of string in Unicode code points
		seg  [4]segment // segments
		best *segment   // shortest segment
	}

	// sSplitter is a Splitter returned by String.Splitter
	// and a Result.
	sSplitter struct {
		sResult        // string and split result
		sp      []span // spans
		rm      []byte // rune map of the string for SplitTo
	}

	// sResult is a Result sometimes returned by sSplitter.SplitTo.
	sResult struct {
		s    string   // string
		head *segment // optimal split
	}
)

// Splitter returns a Splitter calculating an optimal split for s.
// If s is not encodable, Splitter returns an error.
func (s String) Splitter() (Splitter, error) {
	if s.Text == "" {
		return Null{}, nil
	} else if len(s.Text) >= 1<<32 {
		return nil, ErrLongText
	}

	// Get the Classifier function for the modes.
	if s.Charset == nil {
		s.Charset = UTF8
	}
	classify, list, hier := s.Charset.Classifier()
	var mask byte
	for i, v := range list {
		if v >= 0 {
			mask |= 1 << i
		}
	}

	// Scan the string, detect valid encoding modes for each character
	var (
		n, sz  int
		modes  = make([]byte, len(s.Text))
		common = hier
		m, all byte
	)
	for i := 0; i < len(s.Text); i += sz {
		old := m
		m, sz = classify(s.Text[i:])
		if m &= mask; m == 0 {
			return nil, ErrNotEncodable
		}
		modes[i] = m
		if m != old {
			n++
			common &= m
			if latin1HackScan {
				all |= m
			}
		}
	}
	if latin1HackScan && list[Byte] == Latin1 && (common != byteMode ||
		all&kanjiMode != 0 && latin1NeedKanji(modes, hier)) {
		hier &^= kanjiMode // hack, see latin1HackMask
	}
	// If there are modes common for all segments, mask modes within
	// the hierarchy above the lowest common mode.  Mostly useful with
	// single span strings, alphanumeric strings or Latin1+Kanji.
	// In the last case may reduce the number of spans.
	mask ^= (common ^ -common) & hier

	// Populate spans
	sp := make([]span, n)
	old, n, start := byte(0), 0, uint32(0)
	for i, v := range modes {
		if v == 0 {
			continue
		} else if v &= mask; v == 0 {
			panic("qr: internal error")
		} else if v != old {
			if i != 0 {
				sp[n].len = uint32(i) - start
				n++
			}
			old = v
			start = uint32(i)
			seg := &sp[n].seg
			for j := range seg {
				if v&(1<<j) != 0 {
					seg[j].mode = list[j]
				} else {
					seg[j].mode = -1
				}
			}
		}
		sp[n].rlen++
	}
	sp[n].len = uint32(len(modes)) - start
	sp = sp[:n+1]

	return &sSplitter{sResult: sResult{s: s.Text}, sp: sp, rm: modes}, nil
}

// isRuneMode reports whether the mode's EncodedLength is rune-based.
func isRuneMode(mode coding.Mode) bool {
	if mode < ECI {
		return (1<<Kanji|1<<Latin1)>>mode&1 != 0
	} else {
		return 16 < mode.Length(0, 1, 0)
	}
}

const (
	inf1    = 0x6000    // excessive encoded length for one code (>0x5c60)
	inf     = inf1 * 16 // excessive encoded length for structured append
	maxSpan = inf / 3   // max span length for (sSplitter).SplitTo

	_ uint32 = inf1 * maxSpan // check for overflow
)

func (d *segdata) setBits(mode coding.Mode, class int) {
	d.bits = uint32(min(mode.Length(int(d.len), int(d.rlen), class), inf))
	if d.next != nil {
		d.bits += d.next.bits
	}
}

// add adds v to the split before/after next, returning a pointer to
// the segment with the smallest encoded length.
func (v *span) add(next *span, class int) *segment {
	const (
		minHeader = 4 + 8
		maxHeader = 4 + 16
	)
	v.best = nil
	var nbest *segment
	if next != nil {
		nbest = next.best
	}
	for j := range v.seg {
		seg := &v.seg[j]
		if seg.mode < 0 {
			continue
		}
		seg.bits = inf
		// for each seg.mode, one or two segments are created:
		// - if next contains a segment "ns" of the same mode up to
		//   20 bits longer than nbest: merged with ns
		// - if next is nil, does not contain ns, or ns is over 12
		//   bits longer than nbest: linking to nbest
		if next != nil && next.seg[j].mode == seg.mode {
			ns := &next.seg[j]
			if d := ns.bits - nbest.bits; d <= maxHeader {
				seg.len = v.len + ns.len
				seg.rlen = v.rlen + ns.rlen
				seg.next = ns.next
				seg.setBits(seg.mode, class)
				if v.best == nil || seg.bits <= v.best.bits {
					v.best = seg
				}
				if d <= minHeader {
					continue
				}
			}
		}
		c := segdata{len: v.len, rlen: v.rlen, next: nbest}
		c.setBits(seg.mode, class)
		if c.bits < seg.bits {
			seg.segdata = c
			if v.best == nil || c.bits < v.best.bits {
				v.best = seg
			}
		}
	}
	return v.best
}

func (s *sSplitter) Split(class int) (Result, int) {
	// process spans in reverse order
	var next *span
	for i := len(s.sp) - 1; i >= 0; i-- {
		s.sp[i].add(next, class)
		next = &s.sp[i]
	}
	s.head = s.sp[0].best
	return s, int(s.head.bits)
}

// splitOver calculates an optimal split for s, stopping when the
// encoded size exceeds lim bits or at the end.  It returns the index
// of the last processed span.
func (s *sSplitter) splitOver(lim uint32, class int) int {
	var prev *span
	var i int
	// process spans in forward order
	for i = range s.sp {
		if lim < s.sp[i].add(prev, class).bits {
			break
		}
		prev = &s.sp[i]
	}
	return i
}

// reverse reverses the linked list of segments from a forward split.
func (seg *segment) reverse() *segment {
	var next *segment
	for seg != nil {
		seg, next, seg.next = seg.next, seg, next
	}
	return next
}

func (s *sSplitter) SplitTo(lim, class int) (Result, int, Splitter) {
	ll := uint32(min(lim, inf1))
	off := s.splitOver(ll, class)
	this := &s.sp[off] // this span
	if last := this.best; last.bits <= ll {
		s.head = last.reverse()
		return s, int(last.bits), nil
	}
	if maxSpan < this.len {
		s.breakSpan(off)
		off = s.splitOver(ll, class)
		this = &s.sp[off]
	}
	// partition the span s.sp[off], add the beginning to head
	var start uint32 // start of this span
	var prev *span   // pointer to previous span
	if off > 0 {
		for i := range s.sp[:off] {
			start += s.sp[i].len
		}
		prev = &s.sp[off-1]
		s.sp = s.sp[off:] // s.sp[off:] is tail
	}
	sb, sr := this.len, this.rlen  // saved span byte and rune lengths
	var b, r uint32                // partial span byte and rune lengths
	subs := s.rm[start : start+sb] // span substring rune map
	var seg segment                // last segment
	runes := sb != sr

	// estimate last span's partition point
	if sr < 5 {
		if r = sr / 2; r == 0 {
			goto skip
		}
	} else {
		// estimate split point by division using last segment's mode
		const (
			mina   = 5  // mininum bits per alphanumeric rune
			minest = 10 // estimate 1 rune if fewer bits available
			hlen   = 13 // nominal header length (must be <4+10+4)
		)
		avail := ll
		if prev != nil {
			avail -= prev.best.bits
		}
		if avail < mina {
			goto skip
		}
		last := this.best
		if last.len != sb {
			// segment merged, get actual available bits
			for i := range prev.seg {
				if v := &prev.seg[i]; v.mode == last.mode {
					avail = ll - v.bits
					break
				}
			}
		} else {
			avail -= hlen // segment not merged, subtract header
			// avail may underflow.  if small non-negative
			// (originally 13-22 bits) and mode is numeric
			// or alphanumeric, will likely be merged into
			// alphanumeric (<=4 runes) or byte (<=2).
			if prev != nil && avail < minest && last.mode < 2 {
				r = 2
			}
		}
		if int32(avail) >= minest {
			bits := last.bits - hlen // subtract nominal header
			if last.next != nil {
				bits -= last.next.bits
			}
			runes = runes && isRuneMode(last.mode)
			sl, maxr := last.len, sb // segment and span lengths
			if runes {
				sl, maxr = last.rlen, sr
			}
			r = min(avail*sl/bits, maxr-1)
		}
	}
	// set b and r
	if r < 2 {
		b, r = 1, 1 // don't try adding an empty span
	} else if runes {
		rr := r - 1
		for b = 1; rr != 0 && int(b) < len(subs); b++ {
			if subs[b] != 0 {
				rr--
			}
		}
	} else if b = r; sb != sr {
		r = 0
		for _, v := range subs[:b] {
			if v != 0 {
				r++
			}
		}
	}
	for int(b) < len(subs) && subs[b] == 0 {
		b++
	}

	// find the actual partition by stepping one rune at a time
	this.len, this.rlen = b, r
	for this.rlen < sr {
		// as long as split fits in lim, advance
		if ll < this.add(prev, class).bits {
			break
		}
		b, r, seg = this.len, this.rlen, *this.best
		if ll < seg.bits+3 {
			break // 1 rune adds at least 3 bits
		}
		bb := b + 1
		for int(bb) < len(subs) && subs[bb] == 0 {
			bb++
		}
		this.len, this.rlen = bb, r+1
	}
	if seg.len == 0 {
		// if estimate was too large, backtrack until split fits
		for {
			for b--; subs[b] == 0; b-- {
			}
			if r--; r == 0 {
				break
			}
			this.len, this.rlen = b, r
			if last := this.add(prev, class); last.bits <= ll {
				seg = *last
				break
			}
		}
	}

	// seg contains the last segment of result unless b is zero.
	// restore this (first span of tail), return result and tail.
	this.len, this.rlen = sb-b, sr-r
skip:
	var last *segment
	if r != 0 {
		// if head's spans are empty, allocate a new segment,
		// otherwise stuff the last segment into the last span.
		if prev == nil {
			last = &segment{}
		} else if last = &prev.seg[0]; last == seg.next {
			last = &prev.seg[1]
		}
		*last = seg
	} else if prev != nil {
		last = prev.best
	} else {
		return nil, 0, s // not enough space for one rune
	}
	res := &sResult{s: s.s, head: last.reverse()}
	s.s, s.rm = s.s[start+b:], s.rm[start+b:]
	return res, int(last.bits), s
}

// breakSpan breaks an excessively long span.  This can only happen in
// pathological cases with non-standard encodings.
//
//go:noinline
func (s *sSplitter) breakSpan(n int) {
	var start uint32
	for i := range s.sp[:n] {
		start += s.sp[i].len
	}
	for ; maxSpan < s.sp[n].len; n++ {
		s.sp = append(s.sp[:n+1], s.sp[n:]...)
		var b, r uint32
		// if a rune is over maxSpan/4 bytes (32 KB) long,
		// SplitTo may run really slowly.  Don't care.
		end := start + maxSpan*3/4
		for b = start; b < end; b++ {
			if s.rm[b] != 0 {
				r++
			}
		}
		for s.rm[b] == 0 {
			b++
		}
		s.sp[n].len, s.sp[n].rlen = b-start, r
		s.sp[n+1].len -= s.sp[n].len
		s.sp[n+1].rlen -= s.sp[n].rlen
		start = b
	}
}

func (r *sResult) Len() int {
	var n int
	for seg := r.head; seg != nil; seg = seg.next {
		n++
	}
	return n
}

func (r *sResult) Append(a []coding.Segment) []coding.Segment {
	for s, seg := r.s, r.head; seg != nil; seg = seg.next {
		a = append(a, coding.Segment{
			Text: s[:seg.len],
			Mode: seg.mode,
		})
		s = s[seg.len:]
	}
	return a
}

// ModeList is a list of modes used in Charset.
type ModeList [4]coding.Mode

// Classifier returns a bit field of modes in which the first rune in
// the string is encodable, and its length in bytes.  It is used in
// Charset.
type Classifier func(string) (byte, int)

// A Charset is used by String to determine valid encoding Modes for
// each character.
//
// UTF8Charset and ASCIICompatCharset implement Charset for
// ASCII-compatible character encodings.  NewCharset returns a Charset
// for a set of Modes.
//
// Charsets using combinations of predefined Modes are documented
// under UTF8.
type Charset interface {
	// MinLength returns the minimum encoded length of any string of
	// the given length in QR code version size class 0.
	MinLength(int) int

	// Classifier returns a Classifier function, a ModeList and a mode
	// hierarchy mask.
	//
	// m is a list of Modes for encoding numeric, alphanumeric, byte
	// and kanji mode segments.  Some may be set to Disabled.
	//
	// c returns a bit field of modes for the first rune in the string
	// and its length in bytes.  The bit field has bit N (value 1<<N)
	// set for each mode m[N] in which the rune is encodable.
	//
	// mh is a bit field of modes for which no string has a longer
	// encoding in m[A] than in m[B] for any pair of modes where A<B.
	// If unknown or confusing, 0 is always a safe value to return.
	Classifier() (c Classifier, m ModeList, mh byte)
}

// chartbl bits: HKL00ban
//
//	H  0x80  high byte                         Latin1: reset byte mode
//	K  0x40  first byte of Kanji (maybe)       check kanji
//	L  0x20  first byte of Latin-1             Latin1: set byte mode
//	-  0x10  unset
//	b  0x04  byte mode (always set)
//	a  0x02  alphanumeric mode
//	n  0x01  numeric mode
//
// chartbl is used by some Classifier functions to determine in which
// modes the character is encodable.
//
// 4 low bits set the modes for UTF-8 characters.  kanjiMode is always
// unset.
//
// The H bit is set on bytes >=0x80.
//
// The K bit enables Kanji validation.  It is set on 15 bytes that may
// begin a UTF-8 character encodable in Kanji mode.
//
// The L bit sets byteMode for Latin1.  It is set on c2 and c3
// (initial bytes for 0x80-0xbf and 0xc0-0xff, also have K set).
const (
	numMode   = 1 << iota // numeric
	alphaMode             // alphanumeric
	byteMode              // byte          chartbl: always set
	kanjiMode             // kanji         chartbl: unset
	_                     //
	latin1Bit             //               chartbl: Latin-1 if valid rune
	kanjiBit              //               chartbl: maybe Kanji
	highBit               //               chartbl: high byte

	by = byteMode       // ASCII byte
	al = by | alphaMode // alphanumeric
	nu = al | numMode   // numeric
	hi = by | highBit   // high
	ka = hi | kanjiBit  // kanji
	l1 = ka | latin1Bit // latin1 + kanji
)

var chartbl = [256]byte{
	by, by, by, by, by, by, by, by, by, by, by, by, by, by, by, by, // 0x00
	by, by, by, by, by, by, by, by, by, by, by, by, by, by, by, by, // 0x10
	al, by, by, by, al, al, by, by, by, by, al, al, by, al, by, al, // 0x20
	nu, nu, nu, nu, nu, nu, nu, nu, nu, nu, al, by, by, by, by, by, // 0x30
	by, al, al, al, al, al, al, al, al, al, al, al, al, al, al, al, // 0x40
	al, al, al, al, al, al, al, al, al, al, al, by, by, by, by, by, // 0x50
	by, by, by, by, by, by, by, by, by, by, by, by, by, by, by, by, // 0x60
	by, by, by, by, by, by, by, by, by, by, by, by, by, by, by, by, // 0x70
	hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, // 0x80
	hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, // 0x90
	hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, // 0xa0
	hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, // 0xb0
	hi, hi, l1, l1, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, ka, ka, // 0xc0
	ka, ka, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, // 0xd0
	hi, hi, ka, ka, ka, ka, ka, ka, ka, ka, hi, hi, hi, hi, hi, ka, // 0xe0
	hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, hi, // 0xf0
}

// UTF8Charset implements Charset for UTF-8 strings using the given
// Mode for byte mode segments.  The Mode should not be Disabled.
type UTF8Charset coding.Mode

// ASCIICompatCharset implements Charset for ASCII-compatible
// character encodings using the given Mode for kanji mode segments.
// The Mode may be Disabled for eight bit encodings.
type ASCIICompatCharset coding.Mode

type (
	acceptsByte coding.AcceptsFunc
	asciiCompat struct {
		cut coding.CutRuneFunc
		is  coding.AcceptsFunc
	}
)

func numLength(n int) int { return Numeric.Length(n, 0, 0) }

func (UTF8Charset) MinLength(n int) int        { return numLength(n) }
func (ASCIICompatCharset) MinLength(n int) int { return numLength(n) }

func (c UTF8Charset) Classifier() (Classifier, ModeList, byte) {
	var mh byte = numMode | alphaMode | byteMode
	var cf Classifier
	switch coding.Mode(c) {
	case Byte:
		cf = classifyRune
	case Latin1:
		if latin1HackMask {
			mh |= kanjiMode // hack, see latin1HackMask
		}
		cf = classifyLatin1
	default:
		_, is := coding.Mode(c).RuneFilter()
		cf = acceptsByte(is).classify
	}
	return cf, ModeList{Numeric, Alphanumeric, coding.Mode(c), Kanji}, mh
}

func (c ASCIICompatCharset) Classifier() (Classifier, ModeList, byte) {
	cf := classifyByte
	m := ModeList{Numeric, Alphanumeric, Byte, coding.Mode(c)}
	if c >= 0 {
		if cut, is := coding.Mode(c).RuneFilter(); is != nil {
			if cut == nil {
				cut = utf8.DecodeRuneInString
			}
			cf = asciiCompat{cut, is}.classify
		} else {
			cf, _, _ = NewCharset(m, 1).Classifier()
		}
	}
	return cf, m, numMode | alphaMode | byteMode
}

// classify classifies a rune in an ASCII-compatible encoding for
// standard Numeric, Alphanumeric and Byte modes and any kanji mode.
func (a asciiCompat) classify(s string) (byte, int) {
	m := chartbl[s[0]]
	r, sz := a.cut(s)
	if a.is(r) {
		m |= kanjiMode
	}
	return m, sz
}

// classify classifies a UTF-8 rune for standard Numeric, Alphanumeric
// and Kanji modes and any byte mode.
func (is acceptsByte) classify(s string) (byte, int) {
	r, sz := utf8.DecodeRuneInString(s)
	m := chartbl[s[0]] & (numMode | alphaMode | kanjiBit)
	if m&kanjiBit != 0 && coding.IsKanji(r) {
		m |= kanjiMode
	}
	if is == nil || is(r) {
		m |= byteMode
	}
	return m, sz
}

// classifyByte classifies a byte for standard Numeric, Alphanumeric
// and Byte modes.
func classifyByte(s string) (byte, int) { return chartbl[s[0]], 1 }

// classifyRune classifies a rune for standard Numeric, Alphanumeric,
// Byte and Kanji modes.
func classifyRune(s string) (byte, int) {
	m := chartbl[s[0]]
	r, sz := utf8.DecodeRuneInString(s)
	if m&kanjiBit != 0 && coding.IsKanji(r) {
		m |= kanjiMode
	}
	return m, sz
}

// classifyLatin1 classifies a rune for Numeric, Alphanumeric, Latin1
// and Kanji modes.
func classifyLatin1(s string) (byte, int) {
	m := chartbl[s[0]]
	r, sz := utf8.DecodeRuneInString(s)
	if m&highBit != 0 {
		if sz == 1 {
			m = 0
		} else if m = m >> 3; !coding.IsKanji(r) {
			m &^= kanjiMode
		}
	}
	return m, sz
}

/*
latin1HackMask ans latin1HackScan enable conditional compilation of
Latin1 optimisations.

If latin1HackMask is enabled, the mode hierarchy for UTF8AsLatin1
includes Kanji above Latin1.  If all runes are encodable in Latin1,
Kanji will be masked, likely reducing the number of segments.  This
affects strings containing the characters "§¨°±´¶×÷" which belong to
both Latin-1 and QR Kanji character sets but no other Kanji.

Latin1 Mode is preferred over Kanji except for one rune segments in
size class 1 (4+16+8=28 vs. 4+10+13=27 bits).  Masking Kanji increases
the encoded length of such segments in class 1.

If latin1HackScan is enabled, the byte mode is Latin1, all spans are
encodable in byte mode and some in Kanji mode, latin1NeedKanji will
scan the rune mode map for one rune spans encodable in both modes not
adjacent to a Latin1-only span.  If such spans exist, Kanji mode will
be removed from the hierarchy.

SplitMulti may result in such spans at QR code boundaries, losing up
to 2 bits per code if Kanji mode is masked.  I take the hit.
*/
const (
	latin1HackMask = false
	latin1HackScan = latin1HackMask && false
)

/*
latin1NeedKanji reports whether the input string contains a one rune
byte|kanji mode span not adjacent to a byte mode only span.  See
latin1HackMask for details.
*/
func latin1NeedKanji(modes []byte, hier byte) bool {
	var old byte = alphaMode
	var need bool
	for _, v := range modes {
		if v == 0 {
			continue
		} else if v == byteMode|kanjiMode {
			need = old&alphaMode != 0
		} else if need && v&alphaMode != 0 {
			break
		} else {
			need = false
		}
		old = v
	}
	println("hier:", need)
	return need
}

// charset is a Charset returned by NewCharset.
type charset struct {
	cut     coding.CutRuneFunc
	modes   ModeList
	is      [4]coding.AcceptsFunc
	dlShift byte       // digit length shift for MinLength
	cf      Classifier // closure
}

// NewCharset returns a Charset encoding the input string using the
// given modes.
//
// All Modes in m must be compatible with the character encoding of
// the source string.  Modes requiring input in an encoding other than
// UTF-8 or 7 bit ASCII must have CutRune set to the same function.
// Modes are not checked for compatibility.  Encodings accepted by
// predefined Modes are documented under Numeric.
//
// digitLen is the maximum length in bytes of a digit '0'-'9' in the
// source string character encoding: 2 for UTF-16, 4 for UTF-32, 1 for
// most other encodings.  It is used in MinLength.
func NewCharset(m ModeList, digitLen int) Charset {
	c := &charset{cut: utf8.DecodeRuneInString, modes: m}
	if digitLen&(digitLen-1) != 0 {
		c.dlShift++
	}
	for ; digitLen > 1; digitLen >>= 1 {
		c.dlShift++
	}
	for i, v := range m {
		if v >= 0 {
			var cut coding.CutRuneFunc
			if cut, c.is[i] = v.RuneFilter(); cut != nil {
				c.cut = cut
			}
		}
	}
	c.cf = c.classify
	return c
}

func (c *charset) MinLength(n int) int { return numLength(n >> c.dlShift) }

func (c *charset) Classifier() (Classifier, ModeList, byte) {
	return c.cf, c.modes, 0
}

// classify classifies a rune for any modes.
func (c *charset) classify(s string) (byte, int) {
	r, sz := c.cut(s)
	var m byte
	for i, is := range c.is {
		if is == nil || is(r) {
			m |= 1 << i
		}
	}
	return m, sz
}

// Extended Channel Interpretation assignment numbers.
const (
	Latin1ECI   = 3   // ISO 8859-1
	ShiftJISECI = 20  // Shift JIS
	UTF8ECI     = 26  // UTF-8
	BinaryECI   = 899 // 8-bit binary data
)

func setECI(eci uint32) (Data, bool) {
	// pre-allocated strings for standard values
	const s = "\x01\x02\x03\x04\x05\x06\x07\x08" +
		"\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10" +
		"\x11\x12\x13\x14\x15\x16\x17\x18" +
		"\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20" +
		"\x21\x22\x23\x24\x25\x26\x27\x28" // 1-40
	const s170 = "\x80\xaa" // ISO/IEC 646 INV
	const s899 = "\x83\x83" // 8-bit binary data
	seg, ok := Segment{Mode: ECI}, true
	switch {
	case eci == 0:
		return Null{}, true
	case int(eci) <= len(s):
		seg.Text = s[eci-1 : eci]
	case eci == 170:
		seg.Text = s170
	case eci == 899:
		seg.Text = s899
	case eci < 1<<7:
		seg.Text = string(byte(eci))
	case eci < 1<<14:
		seg.Text = string([]byte{0x80 | byte(eci>>8), byte(eci)})
	case eci < 1<<21:
		seg.Text = string([]byte{0xc0 | byte(eci>>16),
			byte(eci >> 8), byte(eci)})
	default:
		ok = false
	}
	return seg, ok
}

// SetECI returns an ECI Mode Segment setting the Extended Channel
// Interpretation assignment number to eci.  It returns a Null if eci
// is 0 and an error if it's equal to or greater than 1<<21.
func SetECI(eci uint32) (Data, error) {
	if seg, ok := setECI(eci); ok {
		return seg, nil
	}
	return nil, ErrECI
}

// MustSetECI is like SetECI but panics on error.
func MustSetECI(eci uint32) Data {
	if seg, ok := setECI(eci); ok {
		return seg
	}
	panic(ErrECI)
}

// ShouldSetECI is like SetECI but returns an unencodable Segment on
// error.
func ShouldSetECI(eci uint32) Data {
	seg, _ := setECI(eci)
	return seg
}

// Text returns a List containing an ECI Mode Segment and a String.
// If eci is 0, only the String is returned.
func Text(text string, c Charset, eci uint32) Data {
	var d Data = String{Text: text, Charset: c}
	if eci != 0 {
		d = List{ShouldSetECI(eci), d}
	}
	return d
}
