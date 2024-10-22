// Copyright 2011 The Go Authors.  All rights reserved.
// Copyright 2024 Vadim Vygonets.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package qr

/*
Bespoke PNG Encoder

The encoder achieves high speeds, and in many scenarios good
compression rates, by limiting its DEFLATE vocabulary to:

  - Encoding literals.
  - Repeating rows of pixels (at scale 1, only header and footer).
  - At scales 4, 8 and 12 above certain size, repeating two pixels for
    horizontal timing pattern.
  - At scales 16 and up, repeating last byte to fill a QR pixel or two
    (two or three at scales below 32).
  - When possible, increasing repeat length by adjusting extra bits.

The literal/length alphabet is limited to bytes that can appear in the
output and preset repeat codes, the distance alphabet to the relevant
subset of 1 byte, two pixel lengths and row length, decreasing the
encoded length of each symbol but restricting possibilities.

At certain combinations of scale, size and border width symbol
frequencies are counted, at others estimated.  At scale 1, QR version
1, border width 0 or 1 fixed Huffman tables are used.

The compounded effect varies by an order of magnitude, as described
under (*Code).PNG (measured without "tEXt" chunk).
*/

import (
	"bytes"
	"encoding/binary"
	"errors"
	"hash/crc32"
	"image/color"
	"io"
)

var (
	ErrArgs       = errors.New("qr: invalid arguments")
	ErrLargeImage = errors.New("qr: image too large")
)

// PNG returns a PNG image displaying the code.
//
// PNG uses a custom encoder tailored to QR codes.
// At c.Scale 8 (default) it runs 40-65x faster than
// calling png.Encode on c.Image(),
// with encoded size ranging from 22-24% smaller
// for QR versions 1 and 2 to about the same at version 40.
// At scale 4, 30-65x faster with 18-28% smaller sizes.
// At other scales performance varies, with sizes of 0.15-1.6x
// at 15-75x speed.
//
// PNG returns nil if the image would be over 64 gigapixels.
func (c *Code) PNG() []byte {
	if w, err := encodePNG(nil, c); err == nil {
		return w.buf.Bytes()
	}
	return nil
}

// EncodePNG writes a PNG image displaying the code to w.
func (c *Code) EncodePNG(w io.Writer) error {
	if w == nil {
		return ErrArgs
	}
	_, err := encodePNG(w, c)
	return err
}

// A pngWriter is a writer for PNG and zlib.
type pngWriter struct {
	bitWriter
	w   io.Writer
	err error

	sym     ctable
	dist    ctable
	adler32 adigest
	start   int
}

const pngHeader = "\x89PNG\r\n\x1a\n"

func encodePNG(ww io.Writer, c *Code) (*pngWriter, error) {
	if !c.isValid() {
		return nil, ErrArgs
	}
	siz := c.Size
	scale := c.Scale
	pix := scale * (siz + c.Border*2)
	if pix > 32767*8 {
		return nil, ErrLargeImage // limit is under 64 gigapixels
	}
	w := pngWriter{w: ww}
	if scale == 8 {
		w.buf.Grow((siz + 8) * (siz + 8) / 4)
	} else if scale == 4 {
		w.buf.Grow((siz + 12) * (siz + 12) / 6)
	}
	pal, rev := c.palette()

	// Header
	w.buf.WriteString(pngHeader)

	// Header block
	binary.BigEndian.PutUint32(w.tmp[0:4], uint32(pix))
	binary.BigEndian.PutUint32(w.tmp[4:8], uint32(pix))
	w.tmp[8] = 1 // 1-bit
	if pal != [2]color.RGBA{} {
		w.tmp[9] = 3 // palette
	} else {
		w.tmp[9] = 0 // gray
	}
	w.tmp[10] = 0 // deflate
	w.tmp[11] = 0 // adaptive filtering
	w.tmp[12] = 0 // no interlace
	w.writeChunk("IHDR", w.tmp[:13])

	// Palette and transparency
	if pal != [2]color.RGBA{} {
		w.tmp[0] = pal[0].R
		w.tmp[1] = pal[0].G
		w.tmp[2] = pal[0].B
		w.tmp[3] = pal[1].R
		w.tmp[4] = pal[1].G
		w.tmp[5] = pal[1].B
		w.writeChunk("PLTE", w.tmp[:6])
		w.tmp[0] = pal[0].A
		w.tmp[1] = pal[1].A
		for a := 2; a > 0; a-- {
			if w.tmp[a-1] != 0xff {
				w.writeChunk("tRNS", w.tmp[:a])
				break
			}
		}
	}

	// Comment
	//w.writeChunk("tEXt", comment)

	// Data
	w.startChunk("IDAT")
	w.writeCode(c, rev)
	w.endChunk()

	// End
	w.writeChunk("IEND", nil)

	if w.w != nil && w.err == nil {
		_, w.err = w.buf.WriteTo(w.w)
	}
	return &w, w.err
}

var comment = []byte("Software\x00QR-PNG http://github.com/unixdj/qr")

func (c *Code) palette() ([2]color.RGBA, bool) {
	const b, w, o = 0x00, 0xff, 0xff // black, white, opaque
	rev := c.Reverse
	if c.Palette != nil {
		rev = !rev
		var pal [2]color.RGBA
		for i := range *c.Palette {
			r, g, b, a := c.Palette[i].RGBA()
			pal[i] = color.RGBA{byte(r >> 8), byte(g >> 8),
				byte(b >> 8), byte(a >> 8)}
		}
		if pal == [2]color.RGBA{{w, w, w, o}, {b, b, b, o}} {
			rev = !rev
		} else if pal != [2]color.RGBA{{b, b, b, o}, {w, w, w, o}} {
			if !rev {
				pal[0], pal[1] = pal[1], pal[0]
			}
			return pal, true
		}
	}
	// if reverse encoding is shorter by over 18 bytes, encode palette
	if !rev {
		lim := int([14]uint16{
			1:  36*4 + 17 - 1,
			8:  26*4 + 17 - 1,
			12: 5*4 + 17 - 1,
			13: 6*4 + 17 - 1,
		}[min(c.Scale, 13)])
		if lim != 0 && lim < c.Size {
			return [2]color.RGBA{{w, w, w, o}, {b, b, b, o}}, true
		}
	}
	return [2]color.RGBA{}, rev
}

const (
	chunkSize = 0x8000 // chunks split after 32 KB
	bufSize   = 0x1000 // buffers flushed in multples of 4 KB
)

func (w *pngWriter) writeChunk(name string, data []byte) {
	w.startChunk(name)
	w.buf.Write(data)
	w.endChunk()
}

func (w *pngWriter) startChunk(name string) {
	w.start = w.buf.Len()
	w.buf.WriteString(name)
	w.buf.WriteString(name)
}

func (w *pngWriter) endChunk() {
	w.endChunkAt(w.buf.Bytes()[w.start:])
}

func (w *pngWriter) endChunkAt(b []byte) {
	binary.BigEndian.PutUint32(b, uint32(len(b)-8))
	binary.BigEndian.PutUint32(w.tmp[0:4], crc32.ChecksumIEEE(b[4:]))
	w.buf.Write(w.tmp[0:4])
}

// checkChunk splits the current chunk if it contains over 32 KB of
// data.  It is called before ending an IDAT chunk and during long
// repeats that occur in large images.
func (w *pngWriter) checkChunk() bool {
	if w.buf.Len()-w.start > 8+chunkSize {
		w.splitChunk()
		return w.err == nil
	}
	return true
}

//go:noinline
func (w *pngWriter) splitChunk() {
	for w.buf.Len()-w.start > 8+chunkSize {
		// End current chunk, add next chunk header.
		prev := w.buf.Bytes()[w.start : w.start+8+chunkSize]
		w.endChunkAt(prev)
		w.buf.Write(prev[:8])
		// Move old chunk's footer and new chunk's header
		// before overflow data.
		buf := w.buf.Bytes()[w.start+8+chunkSize:]
		copy(w.tmp[:12], buf[len(buf)-12:])
		copy(buf[12:], buf)
		copy(buf, w.tmp[:12])
		w.start += chunkSize + 12
	}
	if w.w != nil && w.err == nil {
		n, err := io.CopyN(w.w, &w.buf, int64(w.start&^(bufSize-1)))
		w.start -= int(n)
		w.err = err
	}
}

func (w *pngWriter) writeCode(c *Code, rev bool) {
	const ftNone = 0
	var white byte
	if !rev {
		white = 255
	}

	w.adler32.Reset()
	w.nbit = 0

	// Set repeat lengths.  Row repeat is set for scale>1.
	var r rowParams
	siz, scale, bord := c.Size, c.Scale, c.Border
	r.length = (scale*(siz+bord*2) + 7) / 8
	r.leading = scale * bord / 8
	r.trailing = r.length - (scale*(bord+siz)+7)/8
	if scale != 1 {
		r.lastRowRlen = (scale-1)*(1+r.length) + r.trailing + 1
		r.rowRlen = r.lastRowRlen + r.leading
		if bord == 0 {
			r.lastRowRlen-- // remove ftNone
		} else if !rev && r.leading != 0 {
			r.lastRowRlen++ // add one white byte
		}
	}
	if rev {
		if bs := scale * bord; bs > 1 || bs == 1 && siz > 11*4+17-1 {
			r.tail1Rlen = bs * (1 + r.length)
			r.head1Rlen = r.tail1Rlen + r.leading
			r.tail1Rlen--
		}
		// fmt.Println("head/tail", r.head1Rlen, r.tail1Rlen)
	} else if bord != 0 {
		if r.length > 3 {
			r.head1Rlen = r.length - 1
			r.tail1Rlen = r.head1Rlen
		}
		r.tailRowRlen = (scale*bord - 1) * (1 + r.length)
		r.headRowRlen = r.tailRowRlen + 1 + r.leading
		if r.headRowRlen < 3 {
			r.headRowRlen = 0
		}
	}
	// fmt.Println("rr:", r.rowRlen, r.head1Rlen, r.tail1Rlen, r.headRowRlen, r.tailRowRlen)

	// zlib header
	var cinfo byte // log2 LZ77 window size minus 8, size >= r.length+1.
	for n := r.length >> 8; n != 0; n >>= 1 {
		cinfo++
	}
	w.tmp[0] = cinfo<<4 | 0x08
	w.tmp[1] = 0
	w.tmp[1] += uint8(31 - (uint16(w.tmp[0])<<8+uint16(w.tmp[1]))%31)
	w.buf.Write(w.tmp[0:2])

	// Start flate block.
	var p = irParams{r2row: -1}
	w.writeBits(1, 1) // final block
	if hdr := w.initCodes(c, white, r); hdr != nil {
		w.writeBits(2, 2) // compressed, dynamic Huffman tables
		w.writeBits(uint64(len(w.sym)-257), 5)
		w.writeBits(uint64(len(w.dist)-1), 5)
		w.writeBits(uint64(hdr.clenlen-4), 4)
		w.writeBits(hdr.clens, hdr.clenlen*3)
		for _, v := range hdr.codes {
			w.code(hdr.codex(v.cmd, v.arg))
		}
		p = hdr.p
	} else {
		w.writeBits(1, 2) // compressed, fixed Huffman tables
	}

	// Prepare distance code for row length.  Max 2+13 bits.
	if r.rowRlen|r.headRowRlen != 0 {
		r.rowDist = w.dist.codex(dcode(1 + r.length))
	}

	// White border and first data row leader.
	//   - if Border is 0, the leader is encoded directly
	//   - otherwise, if there's only one row below a certain
	//     length, the row and leader are encoded directly
	//   - otherwise, if rev is true, encode: 0, repeat dist 1
	//   - otherwise:
	//     - if the first row is short, it's encoded directly
	//     - otherwise it's encoded as: 0, white, repeat dist 1
	//     - if there's only one row, leader is encoded directly
	//     - otherwise the rest is encoded as: repeat dist row
	codeNone := w.sym.code(ftNone)
	codeWhite := w.sym.code(uint16(white))
	if bord != 0 {
		w.code(codeNone)
		if r.head1Rlen == 0 {
			w.ncode(codeWhite, r.length)
		} else {
			if !rev {
				w.code(codeWhite)
			}
			w.repeat(r.head1Rlen, w.dist.code(0))
		}
	}
	if r.headRowRlen != 0 {
		w.repeat(r.headRowRlen, r.rowDist)
	} else if r.head1Rlen == 0 || !rev {
		w.code(codeNone)
		w.ncode(codeWhite, r.leading)
	}

	for i := 0; i < scale*bord; i++ {
		w.adler32.WriteNByte(ftNone, 1)
		w.adler32.WriteNByte(white, r.length)
	}

	stride := c.Stride
	bitmap := c.Bitmap
	var row, prev []byte
	slen := scale * bord & 7 // start row with this many white bits
	if scale != 1 || slen|int(white) != 0 {
		row = make([]byte, r.length-r.leading-r.trailing)
		prev = make([]byte, r.length-r.leading-r.trailing)
		if white != 0 {
			for i := range prev {
				prev[i] = white
			}
		}
	}
	addrl := 0 // additional repeat length for the row
	for y := 0; len(bitmap) >= stride; y++ {
		srow := bitmap[:stride]
		bitmap = bitmap[stride:]
		// Raw data.  Bespoke fast encoders for common cases.
		if scale == 8 {
			pbmRow8(row, srow, white)
		} else if scale == 4 {
			pbmRow4(row, srow, white, slen)
		} else if scale == 1 && slen|int(white) == 0 {
			row = srow
		} else {
			pbmRow(row, srow, siz, scale, white, slen)
		}

		// Repeat previous row.
		from, to := 0, len(row)
		if r.rowRlen != 0 {
			// If this and last rows share prefix and/or suffix,
			// increase row repeat length instead of encoding.
			if p.addRowRlen != 0 && (y != 0 || bord != 0) {
				if y != 0 {
					lim := min(p.addRowRlen-addrl, to)
					for from < lim &&
						row[from] == prev[from] {
						from++
					}
				}
				lim := p.addRowRlen
				if y == siz-1 {
					lim = p.addLastRowRlen
				}
				lim = max(to-lim, from)
				for to > lim && row[to-1] == prev[to-1] {
					to--
				}
				if p.ir[0].lo != 0 && from < to {
					if withinRepeat(row, from) {
						from -= w.adjustRR(p, row,
							from, true)
					}
					if withinRepeat(row, to) {
						f := max(from-1, 0)
						to += w.adjustRR(p, row[f:],
							to-f, false)
					}
				}
				addrl += from
			}
			if y != 0 {
				w.repeat(r.rowRlen+addrl, r.rowDist)
				if w.err != nil {
					return
				}
			}
			addrl = len(row) - to
		} else if y != 0 {
			w.code(codeNone)
			w.ncode(codeWhite, r.leading)
		}

		// Encode raw data.
		data := row[from:to]
		if p.ir[0].lo == 0 {
			// scale < ir2scale, simple encoding
			if y == p.r2row {
				off := scale + slen>>2 - from
				for _, z := range data[:off] {
					w.byte(z)
				}
				w.repeat(p.r2len, w.dist.codex(dcode(scale/4)))
				data = data[off+p.r2len:]
			}
			for _, z := range data {
				w.byte(z)
			}
		} else {
			// scale >= ir2scale, inline repeats
			var z byte = ftNone
			if from != 0 {
				z = row[from-1]
			} else if r.leading != 0 {
				z = white
			}
			for i := 0; ; i++ {
				start := i
				for i < len(data) && data[i] == z {
					i++
				}
				n := i - start
				if n >= p.ir[1].lo {
					n = w.iRepeat(n, p, z)
				}
				if n > 0 {
					w.nbyte(z, n)
				}
				if i == len(data) {
					break
				}
				z = data[i]
				w.byte(z)
			}
		}

		if r.rowRlen == 0 {
			w.ncode(codeWhite, r.trailing)
		}

		for i := 0; i < scale; i++ {
			w.adler32.WriteNByte(ftNone, 1)
			w.adler32.WriteNByte(white, r.leading)
			w.adler32.Write(row)
			w.adler32.WriteNByte(white, r.trailing)
		}
		row, prev = prev, row
	}

	if r.rowRlen != 0 {
		w.repeat(r.lastRowRlen+addrl, r.rowDist) // repeat last row
	} else if bord != 0 {
		w.code(codeNone)
	}
	// White border.
	if bord != 0 {
		if !rev && (r.rowRlen == 0 || r.leading == 0) {
			w.code(codeWhite)
		}
		if r.tail1Rlen == 0 {
			w.ncode(codeWhite, r.length-int(white&1))
		} else {
			w.repeat(r.tail1Rlen, w.dist.code(0))
		}
		w.repeat(r.tailRowRlen, r.rowDist)
		for i := 0; i < scale*bord; i++ {
			w.adler32.WriteNByte(ftNone, 1)
			w.adler32.WriteNByte(white, r.length)
		}
	}

	// End of block.
	w.code(w.sym.code(256))
	w.flushBits()

	// adler32
	binary.BigEndian.PutUint32(w.tmp[0:], w.adler32.Sum32())
	w.buf.Write(w.tmp[0:4])
	w.checkChunk()
}

// header describes a DEFLATE dynamic header.
type header struct {
	codes   []lencode // dynamic header code length codes
	ctable            // code length alphabet code table
	clens   uint64    // encoded ordered header code length code lengths
	clenlen byte      // length of clens in 3 bit units, a.k.a. HCLEN+4
	p       irParams  // inline repeat parameters
}

// initCodes initialises Huffman codes for literal/length and distance
// alphabets and returns dynamic header codes.
func (w *pngWriter) initCodes(c *Code, white byte, r rowParams) *header {
	siz := c.Size
	scale := c.Scale
	bord := c.Border
	if scale == 1 && bord <= 1 && siz == 1*4+17 {
		w.sym = fixedSym[:]
		w.dist = fixedDist[:]
		return nil
	}

	// Symbol frequency estimations are listed as counts multiplied by
	// length of de Bruijn sequence and scale.  For this purpose scale
	// is limited to 64, limiting the sum of all values for pixels to:
	//   64 * 4  * 177**2 = 1<<(6+2+7.5*2) = 1<<23  for scale >=8,
	//   3 * 16  * 177**2 < 1<<(2+4+7.5*2) = 1<<21  for scale [2,7],
	//   1 * 256 * 177**2 = 1<<(0+8+7.5*2) = 1<<23  for scale 1.
	// Plus up to 9 repeat values limited to 1<<26 to fit in 32 bits at
	// ridiculous scales.  Have fun with 256K Ultra HD QR codes on ARM.
	var f [nsyms]int

	// Estimate frequencies of literals (0-255).  Other symbols are
	// end of block (256) and known length codes (257-285).
	est := estimateFreqs
	switch scale {
	case 8:
		est = countFreqs8
	case 4:
		if bord&1 == 0 {
			est = countFreqs4
		}
	case 1:
		if 6*4+17-1 < siz && siz < 34*4+17-1 { // ver 6 to 33
			est = estimateFreqs1
		} else {
			est = countFreqs1
		}
	case 2:
		if bord&3 == 0 && siz < 20*4+17-1 {
			est = countFreqs2
		}
	case 3:
		est = countFreqs3
	}
	n := est(c, &f, white)
	f[256] = n // end of block

	// Add repeat lengths.
	var hdr header
	var rs [2]rsyms  // inline repeat symbols
	var dc [3]uint16 // distance codes
	var needDC byte  // distance codes needed
	const (
		dc1, need1 = iota, 1 << iota // 1 byte
		dc2, need2                   // 2 pixels, >1 bytes
		dcr, needr                   // row
	)
	if r.head1Rlen|r.tail1Rlen != 0 {
		addRlenFreq(&f, r.head1Rlen, n)
		addRlenFreq(&f, r.tail1Rlen, n)
		needDC |= need1
	}
	if r.rowRlen|r.headRowRlen != 0 {
		addRlenFreq(&f, r.headRowRlen, n)
		addRlenFreq(&f, r.tailRowRlen, n)
		hdr.p.addRowRlen = addRlenFreq(&f, r.rowRlen, n*(siz-1)).hi -
			r.rowRlen
		hdr.p.addLastRowRlen = addRlenFreq(&f, r.lastRowRlen, n).hi -
			r.lastRowRlen
		dc[dcr], _ = dcode(1 + r.length)
		needDC |= needr
	}
	hdr.p.r2row = -1
	if scale&^12 == 0 && 64 <= (siz-18)*scale {
		// scale=4 ver>4  scale=8 ver>2  scale=12 ver>1
		// Add two pixel repeat for horizontal timing pattern.
		if hdr.p.r2row = c.timingRow(); hdr.p.r2row != -1 {
			hdr.p.r2len = (siz - 14) * scale / 8
			addRlenFreq(&f, hdr.p.r2len, n)
			dc[dc2] = uint16(scale/4 - 1) // dcode(scale / 4)
			needDC |= byte(scale/8 + 1)   // 4->need1 8,12->need2
		}
	} else if scale >= ir2scale {
		// Add inline repeat lenghts.
		needDC |= need1
		var m0, m1, n0, n1 int
		nsq := n * siz * siz
		if scale >= ir1scale {
			m1, m0 = 1, 2
			n1 = nsq * 5 >> 4 // 0.0101010101010100
			n0 = n1 + 1       // 0.0101010101010110
		} else {
			m1, m0 = 2, 3
			n1 = nsq * 7 >> 5 // 0.0011011011011110
			n0 = n1 >> 1      // 0.0001101101101100
		}
		rs[0] = repeatSymbols(scale*m0/8 - 1)
		rs[1] = repeatSymbols(scale*m1/8 - 1)
		hdr.p.ir[0].lrange = rs[0].addFreq(&f, n0)
		hdr.p.ir[1].lrange = rs[1].addFreq(&f, n1)
		f[^white] >>= 1
		f[white] >>= 1
		f[white]-- // slight penalty for repeats at beginning of row
		hdr.p.irCache = &[2]irCache{
			make(irCache, 0, 32),
			make(irCache, 0, 32),
		}
		if hdr.p.addRowRlen != 0 {
			hdr.p.adjCache = &[2][]rrlAdj{
				make([]rrlAdj, 0, 16),
				make([]rrlAdj, 0, 16),
			}
		}
	}

	// Build Huffman codes for literal/length alphabet.
	w.sym = buildCodes(make(ctable, nsyms+ndcodes+nhcodes), f[:], 15)
	hdr.p.ir[0].ebit = rs[0].ebit(w.sym)
	hdr.p.ir[1].ebit = rs[1].ebit(w.sym)

	// Construct distance code symbol table directly.  1 code of 1 bit
	// for one distance, 1 code of 0 bits for none (RFC1951 3.2.7).
	w.dist = w.sym[len(w.sym) : len(w.sym)+int(max(dc[dc2], dc[dcr]))+1]
	if needDC == need1|need2|needr {
		w.dist[dc[dc1]] = code{1, 2} // 10 (appears twice)
		w.dist[dc[dc2]] = code{3, 2} // 11 (appears once)
		w.dist[dc[dcr]] = code{0, 1} // 0  (common)
	} else if needDC != 0 {
		w.dist[dc[needDC&-needDC>>1]] = code{0, 1} // 0
		if needDC &= needDC - 1; needDC != 0 {
			w.dist[dc[needDC>>1]] = code{1, 1} // 1
		}
	}

	// Build header.
	hdr.codes = lenCodes(w.sym[:len(w.sym)+len(w.dist)])

	// Count dynamic header codes and build Huffman codes.
	hlen := f[:nhcodes]
	for i := range hlen {
		hlen[i] = 0
	}
	for _, v := range hdr.codes {
		hlen[v.cmd]++
	}
	hdr.ctable = buildCodes(w.dist[len(w.dist):], hlen, 7)
	hdr.clenlen = 4
	for i := range hdr.ctable {
		if v := hdr.ctable[i].nbit; v != 0 {
			o := revhcorder[i]
			hdr.clens |= uint64(v) << (o * 3)
			hdr.clenlen = max(hdr.clenlen, o+1)
		}
	}
	return &hdr
}

// timingRow returns the row containing the timing pattern, or -1 if
// none was found.
func (c *Code) timingRow() int {
	siz := c.Size
	stride := c.Stride
	b := c.Bitmap
	mask := byte(0xff) << (7 &^ (siz + 1))
loop:
	for r := 6; r < siz; r += siz - 13 {
		s, e := r*stride, (r+1)*stride-2
		for _, v := range b[s+1 : e] {
			if v != 0xaa {
				continue loop
			}
		}
		if (b[e]^0xaa)&mask == 0 && (b[e]^b[e-stride])&^mask != 0 &&
			b[s] != b[s-stride] {
			return r
		}
	}
	return -1
}

// estimateFreqs estimates frequencies of literals for c.Scale above 1
// using a De Bruijn sequence B(2,1) to B(2,4).  Inside the qr code
// proper, any pixel sequence is deemed equally likely, making some bytes
// more likely (e.g., 0x00 and 0xff for most scales).  Bytes that can
// appear as first or last in a row are more likely.
func estimateFreqs(c *Code, f *[nsyms]int, white byte) int {
	//fmt.Println("est")
	// return estimateFreqs(c, f, white)
	scale := c.Scale
	siz := c.Size
	shift := (scale | 8) & -(scale | 8)
	// main body of pixels, except trailing
	steps := scale
	if steps >= ir1scale {
		steps = 8 + steps&7
	}
	sq := siz * siz * steps / 8 // not entirely sure about this
	steps /= shift
	// scale  N*8->B(2,1)  4,6,7,N*8+[1,7]->B(2,2)  5->B(2,3)  2,3->B(2,4)
	idx := byte(scale & 7)
	if idx != 0 && scale > 8 {
		idx = 1
	} else {
		idx = [8]byte{0, 1, 3, 3, 1, 2, 1, 1}[idx]
	}
	db := deBruijn2[idx]
	n := steps * int(db.length)
	slen := c.Border * scale & 7         // start byte quiet zone bits
	elen := (c.Border + siz) * scale & 7 // end byte data bits
	eshift := elen % scale               // end byte partial pixel bits
	emask := ^byte(0xff >> elen)         // end byte data mask
	seadd := siz * steps                 // start/end value to add
	var bits int16
	for db.length != 0 {
		bits = bits&0xff | -int16(db.seq&1)<<8
		db.seq >>= 1
		db.length--
		if slen != 0 {
			f[byte(bits)>>slen^white] += seadd
		}
		if emask != 0 {
			f[byte(bits>>eshift)&emask^white] += seadd
		}
		for i := steps; i != 0; i-- {
			bits >>= shift
			if b := byte(bits); b+1 < 2 {
				f[b] += sq * i
				break
			} else {
				f[b] += sq
			}
		}
	}
	return n
}

// countFreqs8 counts frequencies of literals for c.Scale 8.
func countFreqs8(c *Code, f *[nsyms]int, white byte) int {
	// return estimateFreqs(c, f, white)
	const bitset uint64 = 0x4332322132212110
	var black int
	for _, v := range c.Bitmap[1:] {
		black += int((bitset>>(v<<2&0x3c) + bitset>>(v>>2&0x3c)) & 0xf)
	}
	f[^white] = black - 7
	f[white] = c.Size*c.Size - black
	return 1
}

// countFreqs4 counts frequencies of literals for c.Scale 4 with even
// c.Border.
func countFreqs4(c *Code, f *[nsyms]int, white byte) int {
	// return estimateFreqs(c, f, white)
	var cnt [4]int
	for _, v := range c.Bitmap[1:] {
		for i := 0; i < 4; i++ {
			cnt[v&3]++
			v >>= 2
		}
	}
	f[0x00] = 1
	f[0x00^white] = cnt[0] - (7&^c.Size>>1)*c.Size + 1
	f[0x0f^white] = cnt[1]
	f[0xf0^white] = cnt[2]
	f[0xff^white] += cnt[3]
	return 1
}

// estimateFreqs1 estimates frequencies of literals for c.Scale 1, QR
// versions 6 to 33, using a loop instead of De Bruijn sequence B(2,8).
func estimateFreqs1(c *Code, f *[nsyms]int, white byte) int {
	// return xestimateFreqs1(c, f, white)
	siz := c.Size
	bord := c.Border
	f[0] = siz * 0x100 // ftNone
	f[white] += (bord/8 + (2*bord+siz+7)/8 - (bord+siz+7)/8) * siz * 0x100
	if (bord+siz)&7 == 1 {
		f[0x00^white] += siz * 0x80 // 50% trailing
		f[0x80^white] += siz * 0x80 // 50% trailing
	}
	sq := siz * siz / 8
	slen := bord & 7
	for i := 0; i < 0x100; i++ {
		f[i] += sq                    // mid-row data
		f[byte(i)>>slen^white] += siz // leading
	}
	return 0x100
}

// countFreqs1 counts frequencies of literals for c.Scale 1 when
// shorter encoding of literals is likely to offset increased header
// size: at QR versions below 6 due to many literals being absent from
// the data, and above 33 due to long data length.
func countFreqs1(c *Code, f *[nsyms]int, white byte) int {
	siz := c.Size
	stride := c.Stride
	bord := c.Border
	slen := bord & 7
	f[white] = 2
	f[0] = siz + 1
	// Counting stride+1 bytes per row.  Adjust for true length.
	f[white] += ((2*bord+siz)/8 - stride) * siz
	last := white
	end := stride - 1
	for i, v := range c.Bitmap {
		f[v>>slen^last]++
		last = v<<(8-slen) ^ white
		if i == end {
			f[last]++
			last = white
			end += stride
		}
	}
	return 1
}

// countFreqs2 counts frequencies of literals for c.Scale 2 at QR
// versions below 20, when shorter encoding of literals is likely to
// offset increased header size.
func countFreqs2(c *Code, f *[nsyms]int, white byte) int {
	f[0] = 1
	f[white] = 1
	for _, v := range c.Bitmap {
		v ^= white
		vv := (uint16(v) | uint16(v)<<4) & 0x0f0f
		vv |= vv << 2
		vv = (vv&0x1111 | vv&0x2222<<1) * 3
		f[vv&0xff]++
		f[vv>>8]++
	}
	if c.Size&4 == 0 {
		f[white] -= c.Size
	}
	return 1
}

// countFreqs3 counts frequencies of literals for c.Scale 3.
func countFreqs3(c *Code, f *[nsyms]int, white byte) int {
	var bm = [16]uint16{
		00000, 00007, 00070, 00077, 00700, 00707, 00770, 00777,
		07000, 07007, 07070, 07077, 07700, 07707, 07770, 07777,
	}
	f[0] = 1
	f[white] = 1
	bord := c.Border
	stride := c.Stride
	slen := bord * 3 & 7
	end := stride - 1
	shift := 8 - slen
	eol := uint32(white) << (shift + 16)
	last := eol << 8
	for i, v := range c.Bitmap {
		v ^= white
		last |= (uint32(bm[v>>4])<<12 | uint32(bm[v&0xf])) << shift
		f[last>>24]++
		f[byte(last>>16)]++
		f[byte(last>>8)]++
		last <<= 24
		if i == end {
			last |= eol
			f[last>>24]++
			last <<= 8
			end += stride
		}
	}
	siz := c.Size
	f[white] -= (32 - slen - siz&7*3) / 8 * siz
	return 1
}

type rsyms [3]struct{ sym, lo, extra, n int }

// repeatSymbols returns symbols needed to construct repeats for
// length rlen.
func repeatSymbols(rlen int) rsyms {
	var s rsyms
	if rlen < 3 {
		return s
	}
	var i int
	if rlen > 258 {
		nn := rlen / 258
		if rlen -= nn * 258; rlen < 3 && rlen != 0 {
			s[i].sym, s[i].lo, s[i].n = 264, 10, 1
			i++
			rlen += 258 - 10
			nn--
		}
		if nn != 0 {
			s[i].sym, s[i].lo, s[i].n = 285, 258, nn
			i++
		}
	}
	if rlen != 0 {
		c, ext := lcode(rlen)
		s[i].sym = int(c)
		s[i].lo, s[i].extra = rlen-int(ext.bit), int(ext.nbit)
		s[i].n = 1
	}
	return s
}

// addRlenFreq adds n for each symbol needed to construct repeat
// length rlen to f and returns the range encodable with said symbols.
func addRlenFreq(f *[nsyms]int, rlen, n int) lrange {
	return repeatSymbols(rlen).addFreq(f, n)
}

// addFreq adds n for each symbol in rsym to f and returns the range
// of repeat lengths encodable with rsym.
func (s rsyms) addFreq(f *[nsyms]int, n int) lrange {
	var lo, add int
	for _, v := range s {
		if v.n == 0 {
			break
		}
		f[v.sym] = int(min(int64(f[v.sym])+int64(n)*int64(v.n), 1<<26))
		lo += v.lo * v.n
		if v.extra != 0 {
			add = 1<<v.extra - 1
			if v.sym == 284 && f[285] == 0 {
				add--
			}
		}
	}
	return lrange{lo, lo + add}
}

// ebit returns the encoding length for rsyms, assuming 1 bit distance
// code.
func (s rsyms) ebit(c ctable) int {
	var l int
	for _, v := range s {
		if v.n == 0 {
			break
		}
		l += (int(c[v.sym].nbit) + v.extra + 1) * v.n
	}
	return l
}

// lcode returns the length code and extra bits for rlen.
func lcode(rlen int) (uint16, code) {
	/*
	        Extra               Extra               Extra
	   Code Bits Length(s) Code Bits Lengths   Code Bits Length(s)
	   ---- ---- ------     ---- ---- -------   ---- ---- -------
	    257   0     3       267   1   15,16     277   4   67-82
	    258   0     4       268   1   17,18     278   4   83-98
	    259   0     5       269   2   19-22     279   4   99-114
	    260   0     6       270   2   23-26     280   4  115-130
	    261   0     7       271   2   27-30     281   5  131-162
	    262   0     8       272   2   31-34     282   5  163-194
	    263   0     9       273   3   35-42     283   5  195-226
	    264   0    10       274   3   43-50     284   5  227-257
	    265   1  11,12      275   3   51-58     285   0    258
	    266   1  13,14      276   3   59-66
	*/
	if rlen -= 3; rlen == 0xff {
		return 285, code{}
	} else if rlen&^0xff != 0 {
		panic("qr: invalid repeat length")
	}
	r := uint16(rlen)
	var n uint16
	for 8<<n <= r {
		n++
	}
	// r>>n is [0,7] if n=0, otherwise [4,7].
	return 257 + n<<2 + r>>n, code{r & (1<<n - 1), byte(n)}
}

// dcode returns the distance code and extra bits for dist.
func dcode(dist int) (uint16, code) {
	/*
	        Extra           Extra               Extra
	   Code Bits Dist  Code Bits   Dist     Code Bits Distance
	   ---- ---- ----  ---- ----  ------    ---- ---- --------
	     0   0    1     10   4     33-48    20    9   1025-1536
	     1   0    2     11   4     49-64    21    9   1537-2048
	     2   0    3     12   5     65-96    22   10   2049-3072
	     3   0    4     13   5     97-128   23   10   3073-4096
	     4   1   5,6    14   6    129-192   24   11   4097-6144
	     5   1   7,8    15   6    193-256   25   11   6145-8192
	     6   2   9-12   16   7    257-384   26   12  8193-12288
	     7   2  13-16   17   7    385-512   27   12 12289-16384
	     8   3  17-24   18   8    513-768   28   13 16385-24576
	     9   3  25-32   19   8   769-1024   29   13 24577-32768
	*/
	if dist--; dist&^0x7fff != 0 {
		panic("qr: invalid repeat distance")
	}
	d := uint16(dist)
	var n uint16
	for 4<<n <= d {
		n++
	}
	// d>>n is [0,3] if n=0, otherwise [2,3].
	return n<<1 + d>>n, code{d & (1<<n - 1), byte(n)}
}

const (
	ir2scale = ir1scale / 2 // minimum scale for two-pixel inline repeat
	ir1scale = (3 + 1) * 8  // minimum scale for one-pixel inline repeat
)

// lrange is a range of lengths.
type lrange struct{ lo, hi int }

// irLength is an inline repeat length.
type irLength struct {
	lrange     // minimum and maximum length
	ebit   int // encoded bit length
}

func (l irLength) mul(n int) irLength {
	l.lo *= n
	l.hi *= n
	l.ebit *= n
	return l
}

func (l irLength) add(m irLength) irLength {
	l.lo += m.lo
	l.hi += m.hi
	l.ebit += m.ebit
	return l
}

// irSpec describes how to construct repeat symbols for a certain length.
type irSpec struct {
	n     int    // repeat length
	count [2]int // long and short repeat lengths
	extra int    // length over minimum for count
}

type irCache []irSpec

func (c irCache) find(n int) irSpec {
	for i := range c {
		if c[i].n == n {
			return c[i]
		}
	}
	return irSpec{}
}

func (c *irCache) add(n int, count [2]int, p irParams) irSpec {
	rs := irSpec{
		n:     n,
		count: count,
		extra: n - count[0]*p.ir[0].lo - count[1]*p.ir[1].lo,
	}
	*c = append(*c, rs)
	return rs
}

// iRepeat writes inline repeats for up to n z-valued bytes and
// returns the remaining amount.
func (w *pngWriter) iRepeat(n int, p irParams, z byte) int {
	rs := p.irCache[z&1].find(n)
	if rs.n == 0 {
		rs = p.irCache[z&1].add(n, w.irCount(n, p, z), p)
	}
	dc1 := w.dist.code(0)
	for i, n := range rs.count {
		if n != 0 {
			lo := p.ir[i].lo
			for diff := p.ir[i].hi - lo; n > 0; n-- {
				if rs.extra -= diff; rs.extra < 0 {
					diff += rs.extra
					rs.extra = 0
				}
				w.repeat(lo+diff, dc1)
			}
		}
	}
	return rs.extra
}

// irCount returns counts of inline repeats specified by p for up to
// n z-valued bytes.
func (w *pngWriter) irCount(n int, p irParams, z byte) [2]int {
	lit := int(w.sym[z].nbit) // encoded length of literal
	var count [2]int
	if p.ir[0].ebit >= p.ir[0].hi*lit {
		return count // not worth encoding repeats
	}
	count[0] = (n - 1) / p.ir[0].hi
	if (count[0]+1)*p.ir[0].lo <= n &&
		p.ir[0].ebit <= (n-count[0]*p.ir[0].hi)*lit {
		count[0]++
		if count[0]*p.ir[0].hi >= n {
			return count
		}
	}
	if p.ir[1].ebit >= p.ir[1].hi*lit {
		return count // not worth encoding p.ir[1]
	}
	ebit := 1 << 30
	for c, cont := count, true; cont && c[0] >= 0; c[0]-- {
		lo := c[0] * p.ir[0].lo
		hi := c[0] * p.ir[0].hi
		c[1] = (n - hi - 1) / p.ir[1].hi
		lo += c[1] * p.ir[1].lo
		hi += c[1] * p.ir[1].hi
		if lo+p.ir[1].lo <= n && p.ir[1].ebit <= (n-hi)*lit {
			c[1]++
			hi += p.ir[1].hi
			cont = hi < n
			if !cont && c[0] == count[0] {
				return c
			}
		}
		cont = cont && c[1] < 4
		b := c[0]*p.ir[0].ebit + c[1]*p.ir[1].ebit + max(n-hi, 0)*lit
		if b < ebit {
			count, ebit = c, b
		}
	}
	return count
}

type rowParams struct {
	length      int  // row data length
	leading     int  // leading white bytes (left)
	trailing    int  // trailing white bytes (right)
	rowRlen     int  // row repeat length for distance 1+length, or 0
	lastRowRlen int  // last row repeat length, or 0
	rowDist     code // distance code and extra bits for 1+length
	head1Rlen   int  // head repeat length for distance 1 (top)
	headRowRlen int  // head repeat length for distance 1+length, or 0
	tail1Rlen   int  // tail repeat length for distance 1 (bottom)
	tailRowRlen int  // tail repeat length for distance 1+length, or 0
}

// irParams specifies inline repeat parameters.  addRowRlen is always
// zero for scale 1, other members are set for scales 16 and up.
type irParams struct {
	addRowRlen     int          // maximum additional row repeat length
	addLastRowRlen int          // same for last row
	r2len          int          // repeat length for distance of 2 pixels
	r2row          int          // row for r2len
	ir             [2]irLength  // inline repeat lengths
	irCache        *[2]irCache  // repeat spec cache for bytes 0x00, 0xff
	adjCache       *[2][]rrlAdj // addRowRlen adjustment cache for 00, ff
}

// rrlAdj describes an adjustment for addRowRlen.
type rrlAdj struct {
	n   int       // repeat length
	adj [2]lrange // candidate adjustment ranges
}

// withinRepeat reports whether the byte boundary preceding row[pos]
// falls within a span that may be encoded using repeats.
func withinRepeat(row []byte, pos int) bool {
	return 1 < pos && pos < len(row) &&
		row[pos-1] == row[pos] && row[pos-2] == row[pos]
}

// adjustRR returns the largest number to subtract from additional row
// repeat length to achieve shortest overall encoding.  ends indicates
// that the current row repeat ends at pos, otherwise it begins there.
func (w *pngWriter) adjustRR(p irParams, row []byte, pos int, ends bool) int {
	z := row[pos]
	var s, e int
	for s = pos - 2; s > 0 && row[s-1] == z; s-- {
	}
	for e = pos + 1; e < len(row) && row[e] == z; e++ {
	}
	if s+p.ir[1].lo < e {
		s, e = pos-s-1, e-pos
		if ends {
			s, e = e, s
		}
		for _, v := range w.rrAdjustment(s, p, z) {
			if v.lo <= e {
				return min(v.hi, e)
			}
		}
	}
	return 0
}

// rrAdjustment returns repeat length ranges above n with equal or
// shorter encoding.  If two ranges are returned, the first is longer
// but has equal or shorter encoded length.  If there are fewer than
// two ranges, the remaining ones have zero value.
func (w *pngWriter) rrAdjustment(n int, p irParams, z byte) [2]lrange {
	cache := &p.adjCache[z&1]
	for i := range *cache {
		if (*cache)[i].n == n {
			return (*cache)[i].adj
		}
	}
	rs := p.irCache[z&1].find(n)
	if rs.n == 0 {
		rs.count = w.irCount(n, p, z)
	}

	var c [2]irLength
	if longs := rs.count[0] * p.ir[0].hi; longs < n {
		ebit := rs.count[0]*p.ir[0].ebit + rs.count[1]*p.ir[1].ebit
		if lit := n - longs - rs.count[1]*p.ir[1].hi; lit > 0 {
			ebit += lit * int(w.sym[z].nbit)
		}
		c[1] = p.ir[0].mul(n / p.ir[0].hi) // temp: L long repeats
		// candidate 0: L+1 long repeats
		if c[0] = c[1].add(p.ir[0]); ebit < c[0].ebit {
			c[0] = irLength{}
		}
		// candidate 1: L long, S+1 short repeats
		c[1] = c[1].add(p.ir[1].mul((n-c[1].hi)/p.ir[1].hi + 1))
		// sort so c[0] has higher length and lower or same ebit.
		if ebit < c[1].ebit {
			c[1] = irLength{}
		} else if c[0].ebit == c[1].ebit && c[0].lo <= c[1].hi {
			if c[1].lo <= c[0].hi { // combine overlapping
				c[0].lo = min(c[0].lo, c[1].lo)
				c[0].hi = max(c[0].hi, c[1].hi)
				c[1] = irLength{}
			} else {
				c[0], c[1] = c[1], c[0] // higher length first
			}
		} else if c[0].ebit != 0 {
			if c[1].ebit < c[0].ebit {
				c[0], c[1] = c[1], c[0]
			}
			if c[0].lo <= max(c[1].lo, n) {
				c[1] = irLength{}
			}
		}
	} else {
		c[0] = p.ir[0].mul(rs.count[0])
	}
	var adj [2]lrange
	var j int
	for i := range c {
		if v := c[i].hi - n; v > 0 {
			adj[j].lo = max(c[i].lo-n, 0)
			adj[j].hi = v
			j++
		}
	}
	*cache = append(*cache, rrlAdj{n, adj})
	return adj
}

// A bitWriter is a write buffer for bit-oriented data like deflate.
type bitWriter struct {
	buf  bytes.Buffer
	tmp  [15]byte
	nbit byte
	bit  uint64
}

func (w *bitWriter) flushBits() {
	if n := w.nbit; n > 0 {
		binary.LittleEndian.PutUint64(w.tmp[:], w.bit)
		w.buf.Write(w.tmp[:(n+7)/8])
		w.bit, w.nbit = 0, 0
	}
}

func (w *bitWriter) writeBits(bit uint64, nbit byte) {
	n := w.nbit
	b := w.bit | bit<<n
	n += nbit
	if n >= 64 {
		binary.LittleEndian.PutUint64(w.tmp[:], b)
		w.buf.Write(w.tmp[:8])
		n -= 64
		b = bit >> (nbit - n)
	}
	w.bit, w.nbit = b, n
}

func (w *bitWriter) nxcode(c xcode, n int) {
	for ; n != 0; n-- {
		w.xcode(c)
	}
}

func (w *bitWriter) code(c code)         { w.writeBits(uint64(c.bit), c.nbit) }
func (w *bitWriter) xcode(c xcode)       { w.writeBits(c.bit, c.nbit) }
func (w *bitWriter) ncode(c code, n int) { w.nxcode(c.xcode(), n) }
func (w *pngWriter) byte(x byte)         { w.code(w.sym.code(uint16(x))) }
func (w *pngWriter) nbyte(x byte, n int) { w.ncode(w.sym.code(uint16(x)), n) }

func (w *pngWriter) repeat(n int, d code) {
	if n > 258+2 {
		ld := w.sym.xcodex(285, d)
		// at ridiculous scales check for chunk overflow
		if n > 1<<13 {
			n = w.longRepeat(ld, n)
		}
		nn := (n - 258 - 10) / 258
		w.nxcode(ld, nn)
		n -= nn * 258
		// If n%258 is [3,9] and there's no symbol for it, encode
		// 10 and n-10 like with 259, 230.  Happens with addrl when
		// r.rowRlen%258=[1,2], e.g.: ver 10 (size 57), scale 44.
		for n > 258+9 || n > 258+2 && w.sym[n-4].nbit != 0 {
			w.xcode(ld)
			n -= 258
		}
	}
	if n > 258 {
		w.xcode(w.sym.xcodex(264, d))
		n -= 10
	}
	if n != 0 {
		w.xcode(w.sym.xcodex(lcode(n)).addUnsafe(d))
	}
}

//go:noinline
func (w *pngWriter) longRepeat(ld xcode, n int) int {
	nt := 64 / ld.nbit
	nn := (n - 258 - 10) / (int(nt) * 258)
	n -= nn * int(nt) * 258
	nb := nt * ld.nbit
	for ; ld.nbit < nb; ld.nbit <<= 1 {
		ld.bit |= ld.bit << ld.nbit
	}
	ld.bit &= 1<<nb - 1
	ld.nbit = nb
	for nn > 0 && w.checkChunk() {
		tt := min((w.start+chunkSize+8-w.buf.Len())*8/int(nb)+1, nn)
		w.nxcode(ld, tt)
		nn -= tt
	}
	return n
}

type adigest struct {
	a, b uint32
}

func (d *adigest) Reset() { d.a, d.b = 1, 0 }

const amod = 65521

func aupdate(a, b uint32, pi byte, n int) (aa, bb uint32) {
	// TODO(rsc): 6g doesn't do magic multiplies for b %= amod,
	// only for b = b%amod.

	// invariant: a, b < amod
	if pi == 0 {
		b += uint32(n%amod) * a
		b = b % amod
		return a, b
	}

	// n times:
	//	a += pi
	//	b += a
	// is same as
	//	b += n*a + n*(n+1)/2*pi
	//	a += n*pi
	m := uint32(n)
	b += (m % amod) * a
	b = b % amod
	b += (m * (m + 1) / 2) % amod * uint32(pi)
	b = b % amod
	a += (m % amod) * uint32(pi)
	a = a % amod
	return a, b
}

func afinish(a, b uint32) uint32 {
	return b<<16 | a
}

func (d *adigest) Write(p []byte) {
	for _, pi := range p {
		d.a, d.b = aupdate(d.a, d.b, pi, 1)
	}
}

func (d *adigest) WriteNByte(pi byte, n int) {
	d.a, d.b = aupdate(d.a, d.b, pi, n)
}

func (d *adigest) Sum32() uint32 { return afinish(d.a, d.b) }
