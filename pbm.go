// Copyright 2011 The Go Authors.  All rights reserved.
// Copyright 2024 Vadim Vygonets.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package qr

import (
	"bufio"
	"encoding/binary"
	"io"
	"strconv"
)

// EncodePBM writes a Portable Bit Map image displaying the code to w,
// for use with netpbm.  EncodePBM disregards c.Palette, as other PNM
// formats are not supported.
func (c *Code) EncodePBM(w io.Writer) error {
	if !c.isValid() {
		return ErrArgs
	}
	b := bufio.NewWriter(w)
	siz := c.Size
	scale := c.Scale
	bord := c.Border
	length := scale * (siz + bord*2)
	ls := strconv.Itoa(length)
	if _, err := b.WriteString("P4\n" + ls + " " + ls + "\n"); err != nil {
		return err
	}
	row := make([]byte, (length+7)/8)
	var white byte
	if c.Reverse {
		white = 255
		for i := range row {
			row[i] = white
		}
	}
	for i := 0; i < scale*bord; i++ {
		if _, err := b.Write(row); err != nil {
			return err
		}
	}
	data := row[scale*bord/8 : (scale*(siz+bord)+7)/8]
	slen := scale * bord & 7
	stride := c.Stride
	bitmap := c.Bitmap
	for len(bitmap) >= stride {
		srow := bitmap[:stride]
		bitmap = bitmap[stride:]
		// Raw data.  Bespoke fast encoders for common cases.
		if scale == 8 {
			pbmRow8(data, srow, white)
		} else if scale == 4 {
			pbmRow4(data, srow, white, slen)
		} else if scale == 1 && slen|int(white) == 0 {
			copy(data, srow)
		} else {
			pbmRow(data, srow, siz, scale, white, slen)
		}
		for i := 0; i < scale; i++ {
			if _, err := b.Write(row); err != nil {
				return err
			}
		}
	}
	if bord != 0 {
		for i := range data {
			data[i] = white
		}
		for i := 0; i < scale*bord; i++ {
			if _, err := b.Write(row); err != nil {
				return err
			}
		}
	}
	return b.Flush()
}

// pbmRow8 encodes a row of QR data pixels in PBM format at scale 8.
func pbmRow8(row, srow []byte, white byte) {
	var b uint64
	for _, v := range srow {
		v ^= white
		for i := 0; i < 8; i++ {
			b = b<<8 | uint64(-(v & 1))
			v >>= 1
		}
		if len(row) < 8 {
			break
		}
		binary.LittleEndian.PutUint64(row, b)
		row = row[8:]
	}
	if len(row) > 4 {
		binary.LittleEndian.PutUint32(row, uint32(b))
		b >>= 32
		row = row[4:]
	}
	for i := range row {
		row[i] = byte(b)
		b >>= 8
	}
}

// pbmRow4 encodes a row of QR data pixels in PBM format at scale 4.
func pbmRow4(row, srow []byte, white byte, slen int) {
	var b uint32
	var last uint16
	slen >>= 2
	for _, v := range srow {
		last |= uint16(v)
		b = uint32(byte(last>>slen)^white) * 01001001 & 0300070007 *
			0111 & 0x11111111 * 0xf
		last <<= 8
		if len(row) < 4 {
			break
		}
		binary.BigEndian.PutUint32(row, b)
		row = row[4:]
	}
	for i := range row {
		row[i] = byte(b >> 24)
		b <<= 8
	}
}

// pbmRow encodes a row of QR data pixels in PBM format.
func pbmRow(row, srow []byte, siz, scale int, white byte, slen int) {
	j := 0
	z := white
	if scale == 1 {
		for _, v := range srow {
			row[j] = z ^ v>>slen
			z = v<<(8-slen) ^ white
			j++
		}
		if j < len(row) {
			row[j] = z
		}
	} else {
		nz := slen
		for _, v := range srow {
			v ^= white
			for i := 0; i < 8; i++ {
				bits := byte(int8(v) >> 7)
				v <<= 1
				shift := min(8-nz, scale)
				z = z<<shift | bits>>(8-shift)
				for nz += scale; nz >= 8; nz -= 8 {
					if j >= len(row) {
						return
					}
					row[j] = z
					z = bits
					j++
				}
			}
		}
	}
}
