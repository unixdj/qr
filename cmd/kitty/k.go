package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
	"syscall"

	"github.com/mattn/go-isatty"
	"github.com/pborman/getopt/v2"
	"github.com/unixdj/qr"
	"github.com/unixdj/qr/coding"
	"github.com/unixdj/qr/split"
)

var g struct {
	fn      string         // filename
	lev     coding.Level   // QR correction level
	ver     coding.Version // QR version
	eci     int            // ECI segment value
	format  int            // output file format
	copies  int            // data size redundancy
	nokanji bool           // kanji mode disabled
	omit    bool           // omit data
}

func printUsage(w io.Writer) {
	cl := getopt.CommandLine
	prog := cl.Program()
	ul := make([]string, 1, 4)
	ul[0] = cl.UsageLine() + " [string ...]"
	ml := max(70-len("Usage: ")-1-len(prog), 0)
	for i := 0; len(ul[i]) > ml; i++ {
		s := ul[i]
		n := ml - 1
		for n > 0 && (s[n] != ' ' || s[n+1] != '[') {
			n--
		}
		ul = append(ul, s[n+1:])
		ul[i] = s[:max(n, 0)]
		ml = 60
	}
	fmt.Fprint(w, "QR code generator\nUsage: ", prog, " ",
		strings.Join(ul, "\n          "), `
If no string is given, data is read from standard input and the final
newline is stripped.

`)
	var b bytes.Buffer
	cl.PrintOptions(&b)
	bb := b.Bytes()
	/*
		if n := bytes.Index(bb, []byte(" [-1]")); n != 1 {
			w.Write(bb[:n])
			bb = bb[n+len(" [-1]"):]
		}
	*/
	w.Write(bb)
}

type opt func()

func (opt) String() string                    { return "" }
func (o opt) Set(string, getopt.Option) error { o(); return nil }

func usage() {
	printUsage(os.Stderr)
	os.Exit(2)
}

func help() {
	printUsage(os.Stdout)
	os.Exit(0)
}

var formats = []string{"png", "utf8"}

var encoders = [...]func(*qr.Code, io.Writer) error{
	(*qr.Code).EncodePNG,
	func(c *qr.Code, w io.Writer) error {
		_, err := fmt.Fprint(w, c)
		return err
	},
}

func parseFlags() {
	getopt.SetUsage(usage)
	// getopt.SetParameters("[string ...]")
	getopt.Flag(opt(help), 'h', "show this help").SetFlag()
	padsz := getopt.Unsigned('p', 2, &getopt.UnsignedLimit{0, 8, 0, 0},
		"padding size as multiples of data")
	fno := getopt.Flag(&g.fn, 'o', `output file, or "-" for `+
		`standard output`, "file")
	eci := getopt.Signed('E', 0, &getopt.SignedLimit{0, 21, 0, 999999},
		"encode ECI segment with the given value", "eci")
	ver := getopt.Unsigned('v', 1, &getopt.UnsignedLimit{0, 8, 1, 40},
		"minimum QR code version", "ver")
	lev := getopt.Enum('l',
		[]string{"l", "m", "q", "h", "L", "M", "Q", "H"}, "l",
		"error correction level, lowest to highest", "l|m|q|h")
	ff := getopt.Enum('t', formats, "", `output format, one of: `+
		strings.Join(formats, ", ")+
		`; if no -o is given and standard output is a TTY, `+
		`default is utf8, otherwise png`, "type")
	getopt.Flag(&g.nokanji, 'K', "disable kanji mode")
	getopt.Flag(&g.omit, 'O', "omit data")

	getopt.Parse()
	g.copies = int(*padsz) + 1
	g.ver = coding.Version(*ver)
	g.lev = coding.Level(strings.Index("lmqhLMQH", *lev) & 3)
	if getopt.IsSet('E') {
		g.eci = int(*eci)
	} else {
		g.eci = -1
	}
	if *ff != "" {
		for i, v := range formats {
			if *ff == v {
				g.format = i
				break
			}
		}
	} else if !fno.Seen() && isatty.IsTerminal(uintptr(syscall.Stdout)) {
		g.format = 1
	}
	if g.fn == "-" {
		g.fn = ""
	}
}

/*
 █▄   ▄█
▄▀ ▀▀▀ ▀▄
█ ▀ ▄ ▀ █
▀▄     ▄▀
  ▀▀▀▀▀
 ▄     ▄
 █▀▄▄▄▀█
█ ▄   ▄ █
█   ▀   █
 ▀▄▄▄▄▄▀
*/

const (
	catMul = 2 | 2<<10
	cat0   = catMul * 0000 // |████████████████████|
	cat1   = catMul * 0202 // |████  ██████████  ██|
	cat2   = catMul * 0306 // |████    ██████    ██|
	cat3   = catMul * 0272 // |████  ██      ██  ██|
	cat4   = catMul * 0401 // |██  ██████████████  |
	cat5   = catMul * 0505 // |██  ██  ██████  ██  |
	cat6   = catMul * 0421 // |██  ██████  ██████  |
	cat7   = catMul * 0401 // |██  ██████████████  |
	cat8   = catMul * 0202 // |████  ██████████  ██|
	cat9   = catMul * 0174 // |██████          ████|
)

var cat = [10][5]byte{
	{
		cat0 >> 2 & 0xff, cat0 >> 4 & 0xff,
		cat0 >> 6 & 0xff, cat0 >> 8 & 0xff, cat0 & 0xff,
	},
	{
		cat1 >> 2 & 0xff, cat1 >> 4 & 0xff,
		cat1 >> 6 & 0xff, cat1 >> 8 & 0xff, cat1 & 0xff,
	},
	{
		cat2 >> 2 & 0xff, cat2 >> 4 & 0xff,
		cat2 >> 6 & 0xff, cat2 >> 8 & 0xff, cat2 & 0xff,
	},
	{
		cat3 >> 2 & 0xff, cat3 >> 4 & 0xff,
		cat3 >> 6 & 0xff, cat3 >> 8 & 0xff, cat3 & 0xff,
	},
	{
		cat4 >> 2 & 0xff, cat4 >> 4 & 0xff,
		cat4 >> 6 & 0xff, cat4 >> 8 & 0xff, cat4 & 0xff,
	},
	{
		cat5 >> 2 & 0xff, cat5 >> 4 & 0xff,
		cat5 >> 6 & 0xff, cat5 >> 8 & 0xff, cat5 & 0xff,
	},
	{
		cat6 >> 2 & 0xff, cat6 >> 4 & 0xff,
		cat6 >> 6 & 0xff, cat6 >> 8 & 0xff, cat6 & 0xff,
	},
	{
		cat7 >> 2 & 0xff, cat7 >> 4 & 0xff,
		cat7 >> 6 & 0xff, cat7 >> 8 & 0xff, cat7 & 0xff,
	},
	{
		cat8 >> 2 & 0xff, cat8 >> 4 & 0xff,
		cat8 >> 6 & 0xff, cat8 >> 8 & 0xff, cat8 & 0xff,
	},
	{
		cat9 >> 2 & 0xff, cat9 >> 4 & 0xff,
		cat9 >> 6 & 0xff, cat9 >> 8 & 0xff, cat9 & 0xff,
	},
}

// fill returns a QR Code with bitmap filled with the pattern.
func fill(v coding.Version) *qr.Code {
	siz := int(v*4 + 17)
	stride := (siz + 7) / 8
	b := make([]byte, siz*stride)
	c := qr.Code{Bitmap: b, Size: siz, Stride: stride}
	for _, v := range cat {
		l := b[:stride]
		b = b[stride:]
		for n := copy(l, v[:]); n < len(l); n += copy(l[n:], l[:n]) {
		}
	}
	b = c.Bitmap
	for n := len(cat) * stride; n < len(b); n += copy(b[n:], b[:n]) {
	}
	return &c
}

type bits struct {
	bit byte
	b   []byte
}

func (b *bits) full() bool {
	return b.bit == 0 && len(b.b) == cap(b.b)
}

func (b *bits) add(v bool) {
	bit := b.bit
	if bit == 0 {
		if len(b.b) == cap(b.b) {
			return
		}
		if v {
			bit = 0x80
		}
		b.b = append(b.b, bit)
		bit = 0x80
	} else if v {
		b.b[len(b.b)-1] |= bit
	}
	b.bit = bit >> 1
}

// extract extracts db bits from d according to the map m.
func extract(d, m *qr.Code, db int) []byte {
	b := bits{b: make([]byte, 0, db)}
	for x := d.Size - 2; !b.full(); x -= 2 {
		for y := d.Size - 1; y >= 0; y-- {
			if !m.Black(x+1, y) {
				b.add(d.Black(x+1, y))
			}
			if !m.Black(x, y) {
				b.add(d.Black(x, y))
			}
		}
		if b.full() {
			break
		}
		if x -= 2; x == 5 {
			x--
		}
		for y := 0; y < d.Size; y++ {
			if !m.Black(x+1, y) {
				b.add(d.Black(x+1, y))
			}
			if !m.Black(x, y) {
				b.add(d.Black(x, y))
			}
		}
	}
	return b.b
}

// unpermute returns bytes from the permuted sequence b.
func unpermute(b []byte, blocks int) []byte {
	if blocks == 1 {
		return b
	}
	dst := make([]byte, len(b))
	extra := len(b) / blocks * blocks
	last := len(b) - blocks
	var j int
	for i := 0; i < blocks; i++ {
		for k := i; k < extra; k += blocks {
			dst[j] = b[k]
			j++
		}
		if last >= extra {
			dst[j] = b[last]
			j++
		}
		last++
	}
	return dst
}

// ones lists the number of bits set in each byte.
var ones = [256]byte{
	0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, // 0x00
	1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, // 0x10
	1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, // 0x20
	2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, // 0x30
	1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, // 0x40
	2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, // 0x50
	2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, // 0x60
	3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, // 0x70
	1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, // 0x80
	2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, // 0x90
	2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, // 0xa0
	3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, // 0xb0
	2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, // 0xc0
	3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, // 0xd0
	3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, // 0xe0
	4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8, // 0xf0
}

func main() {
	log.SetFlags(0)
	parseFlags()

	var text string
	if args := getopt.Args(); len(args) != 0 {
		text = strings.Join(args, " ")
	} else {
		var b strings.Builder
		if _, err := io.Copy(&b, os.Stdin); err != nil {
			log.Fatalln(err)
		}
		text, _ = strings.CutSuffix(
			strings.ReplaceAll(b.String(), "\r\n", "\n"), "\n")
	}
	var cs split.Charset
	if g.nokanji {
		cs = split.ASCIICompat
	}
	data := split.Text(text, cs, g.eci)

	// split multiple copies of text
	list := make(split.List, g.copies)
	for i := range list {
		list[i] = data
	}
	seg, ver, err := split.Split(list, g.lev, split.QR)
	if err != nil {
		log.Fatalln(err)
	}
	seg = seg[:len(seg)/g.copies] // text only
	class := ver.SizeClass()
	// if version is below requested, set version
	if ver < g.ver {
		// if size class differs, split for the given version's class
		cc := class
		if class = g.ver.SizeClass(); class != cc {
			s, _, _ := data.Splitter() // will succeed
			r, _ := s.Split(class)
			seg = r.Append(seg[:0])
		}
		ver = g.ver
	}

	db := ver.DataBits(g.lev) / 8
	b := coding.NewBits(ver, g.lev)

	// Find number of blocks for (ver, g.lev):
	// set b[1] to sentinel, permute, find the sentinel in bytes
	b.Add(db)[1] = 1
	b.AddCheckBytes(ver, g.lev)
	bits := b.Permute(ver, g.lev)
	bb := bits.Bytes()
	var blocks int
	for i := range bb {
		if bb[i] != 0 {
			blocks = i
			break
		}
	}

	// encide data and termination bits
	b.Reset()
	for i := range seg {
		seg[i].Encode(b, class)
	}
	b.Write(0, 4)     // terminator
	nb := b.Bits()    // number of data bits
	b.Write(0, -nb&7) // pad to byte boundary
	bb = b.Bytes()

	p, err := coding.NewPlan(ver, g.lev)
	if err != nil {
		log.Fatalln(err)
	}

	// Fill QR code with cat faces, extract and unpermute data bytes
	pmap := &qr.Code{
		Bitmap: p.Map,
		Size:   p.Size,
		Stride: (p.Size + 7) >> 3,
	}
	patb := unpermute(extract(fill(ver), pmap, db), blocks)

	// Extract and unpermute data bytes from mask patterns.
	// Choose mask that, xored with data, would be closest to the
	// cat faces pattern.
	c := &qr.Code{
		Size:   p.Size,
		Stride: (p.Size + 7) >> 3,
		Scale:  4,
		Border: 4,
	}
	var best, bpat []byte
	diff := nb
	for i := range p.Pattern {
		c.Bitmap = p.Pattern[i]
		// extract mask pattern bytes
		mpb := unpermute(extract(c, pmap, db), blocks)
		for j, v := range patb {
			mpb[j] ^= v // xor mask bytes with cat faces
		}
		// compute difference between masked cat faces and data
		p := 0
		for j, v := range bb[:nb>>3] {
			p += int(ones[mpb[j]^v])
		}
		if n := -nb & 7; n != 0 {
			p += int(ones[(mpb[nb>>3]^bb[nb>>3])>>n])
		}
		// println(i, p, nb)
		if p < diff {
			diff = p        // difference
			best = mpb      // best masked cat faces
			bpat = c.Bitmap // best pattern
		}
	}

	// set padding bits to masked cat faces
	if n := -nb & 7; n != 0 {
		bb[nb>>3] |= best[nb>>3] & (1<<n - 1)
	}
	nbx := len(bb)
	copy(b.Add(db-nbx), best[nbx:])

	// Construct the code: add check bytes, permute,
	// serialise straight onto the pattern.
	b.AddCheckBytes(ver, g.lev)
	if g.omit {
		copy(b.Bytes()[:nbx], best)
	}
	// copy(b.Bytes()[21:nbx], best[21:nbx])
	p.Serialise(bpat, b.Permute(ver, g.lev))
	c.Bitmap = bpat

	// write code
	var w = os.Stdout
	if g.fn != "" {
		if w, err = os.OpenFile(g.fn,
			os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666); err != nil {
			log.Fatalln(err)
		}
	}
	if err = encoders[g.format](c, w); err != nil {
		log.Fatalln(err)
	}
	if g.fn != "" {
		if err = w.Close(); err != nil {
			log.Fatalln(err)
		}
	}
}
