package main

import (
	"bytes"
	"fmt"
	"image/color"
	"image/png"
	"io"
	"log"
	"os"
	"path"
	"strconv"
	"strings"
	"syscall"

	"github.com/unixdj/qr"
	"github.com/unixdj/qr/coding"
	"github.com/unixdj/qr/split"

	"github.com/mattn/go-isatty"
	"github.com/pborman/getopt/v2"
)

// Undocumented features:
//   -t vis|visi: compression visualisation
//   16 times -f: test image

var g = struct {
	scale    int             // scale
	border   int             // quiet zone
	palette  *[2]color.Color // palette
	rev      bool            // reverse colours
	fn       string          // filename
	fext     string          // filename suffix
	lev      qr.Level        // QR correction level
	ver      coding.Version  // QR version
	format   int             // output file format
	cx       int             // randr source X coordinate index in inc
	inc      [2]int          // randr source X,Y coordinate increments
	eci      int             // ECI segment value
	fnc1     int             // FNC1 codeword value
	bg, fg   rgba            // colour
	colSet   bool            // colour set
	eciflag  bool            // ECI flag
	fnc1flag bool            // FNC1 flag
	latin1   bool            // Latin-1 byte mode
	sjis     bool            // Shift JIS input
	nokanji  bool            // kanji mode disabled
	eightBit bool            // 8 bit input
	byteOnly bool            // byte mode only
	upper    bool            // uppercase
	multi    bool            // structured append
	cformat  split.Format    // QR / Micro / Either
}{
	inc:  [2]int{1, 1},
	bg:   rgba{0xff, 0xff, 0xff, 0xff},
	fg:   rgba{0x00, 0x00, 0x00, 0xff},
	fnc1: -1,
}
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
newline is stripped.  Defaults: UTF-8 input, no conversion, kanji mode
segments enabled, no ECI segment.

`)
	var b bytes.Buffer
	cl.PrintOptions(&b)
	bb := b.Bytes()
	if n := bytes.Index(bb, []byte(" [256]")); n != 1 {
		w.Write(bb[:n])
		bb = bb[n+len(" [256]"):]
	}
	if n := bytes.Index(bb, []byte(" [-1]")); n != 1 {
		w.Write(bb[:n])
		bb = bb[n+len(" [-1]"):]
	}
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

func version() {
	fmt.Println(`qr version 0.8.0
Copyright (c) 2011 The Go Authors
Copyright (c) 2025 Vadim Vygonets`)
	os.Exit(0)
}

func flip() {
	g.inc[0] = -g.inc[0]
}

func rotate() {
	g.cx ^= 1
	m := g.inc[0] * g.inc[1]
	g.inc[0] *= m
	g.inc[1] *= -m
}

func noKanji() {
	g.eightBit = g.nokanji
	g.nokanji = true
}

func cformat() {
	if g.cformat < qr.Either {
		g.cformat++
	}
}

type rgba struct {
	R, G, B, A uint8
}

func (c *rgba) String() string {
	if *c == (rgba{0x00, 0x00, 0x00, 0xff}) {
		return "black"
	} else if *c == (rgba{0xff, 0xff, 0xff, 0xff}) {
		return "white"
	} else if c.A == 0xff {
		return fmt.Sprintf("%02x%02x%02x", c.R, c.G, c.B)
	} else {
		return fmt.Sprintf("%02x%02x%02x%02x", c.R, c.G, c.B, c.A)
	}
}

func (c *rgba) Set(s string, _ getopt.Option) error {
	g.colSet = true
	var ok bool
	if *c, ok = rgb[strings.ToLower(strings.ReplaceAll(s, " ", ""))]; ok {
		return nil
	}
	n, err := strconv.ParseUint(s, 16, 32)
	if err != nil {
		return fmt.Errorf("%q: bad colour spec", s)
	}
	switch len(s) {
	case 3:
		n = n<<4 | 0xf
		fallthrough
	case 4:
		var nn uint64
		for i := 0; i < 4; i++ {
			nn <<= 8
			nn |= n >> 12 & 0xf * 0x11
			n <<= 4
		}
		n = nn
	case 6:
		n = n<<8 | 0xff
	case 8:
	default:
		return fmt.Errorf("%q: bad colour spec", s)
	}
	c.R, c.G, c.B, c.A = uint8(n>>24), uint8(n>>16), uint8(n>>8), uint8(n)
	return nil
}

var formats = []string{
	"png", "pngi", "PNG", "PNGi", "pbm", "pbmi", "eps", "epsi",
	"utf8", "utf8i", "ascii", "asciii",
}

var encoders = [...]func(*qr.Code, io.Writer) error{
	(*qr.Code).EncodePNG,
	func(c *qr.Code, w io.Writer) error { return png.Encode(w, c.Image()) },
	(*qr.Code).EncodePBM,
	eps,
	func(c *qr.Code, w io.Writer) error {
		_, err := fmt.Fprint(w, c)
		return err
	},
	ascii,
}

func parseFlags() {
	getopt.SetUsage(usage)
	// getopt.SetParameters("[string ...]")
	getopt.Flag(opt(help), 'h', "show this help").SetFlag()
	getopt.Flag(opt(version), 'V', "print version and copyright").SetFlag()
	getopt.FlagLong(&g.bg, "background", 'B', `background colour; see -F`,
		"RGB[A]|name")
	getopt.FlagLong(&g.fg, "foreground", 'F', `foreground colour `+
		`as 3, 4, 6 or 8 hex digits or X11 rgb.txt colour name; `+
		`only for types png[i], PNG[i] and eps[i]`, "RGB[A]|name")
	getopt.Flag(opt(flip), 'f', `flip code horizontally; `+
		`to flip vertically, use "-frr"`).SetFlag()
	getopt.Flag(opt(rotate), 'r', `rotate code 90° counterclockwise; `+
		`-r and -f may be given multiple times, `+
		`order matters: "-fr" = "-rfrr" = "-rrrf"`).SetFlag()
	getopt.Flag(opt(noKanji), 'K', "disable kanji mode; "+
		`-KK: treat input as 8 bit for -S`).SetFlag()
	getopt.Flag(&g.latin1, '1',
		"convert byte mode segments to Latin-1")
	getopt.Flag(&g.byteOnly, '8', "encode entire data in byte mode")
	getopt.Flag(&g.sjis, 'k', "Shift JIS input")
	getopt.Flag(&g.upper, 'i', `ignore case, convert input to uppercase`)
	getopt.Flag(&g.multi, 'S', `encode structured append symbols `+
		`(multiple QR codes); -v should be specified`)
	getopt.Flag(opt(cformat), 'M', "encode a Micro QR code; "+
		"-MM: only if data fits").SetFlag()
	getopt.Flag(&g.border, 'm', `quiet zone pixels [4 (2 for Micro)]`,
		"margin")
	fno := getopt.Flag(&g.fn, 'o', `output file, or "-" for `+
		`standard output; with -S, "-01", "-02" etc. is appended `+
		`to the filename before suffix`, "file")
	getopt.Flag(&g.eciflag, 'e', "encode ECI segment setting "+
		"character encoding according to -1 and -k flags")
	getopt.Flag(&g.fnc1flag, 'c', "set FNC1 in first position")
	fnc1 := getopt.Unsigned('C', 256, &getopt.UnsignedLimit{0, 8, 0, 0},
		"set FNC1 in second position to the given value", "code")
	eci := getopt.Signed('E', -1, &getopt.SignedLimit{0, 21, 0, 999999},
		"encode ECI segment with the given value; overrides -e", "eci")
	ver := getopt.Unsigned('v', 1, &getopt.UnsignedLimit{0, 8, 1, 40},
		"QR code version for structured append", "ver")
	lev := getopt.Enum('l',
		[]string{"l", "m", "q", "h", "L", "M", "Q", "H"}, "l",
		"error correction level, lowest to highest", "l|m|q|h")
	scale := getopt.Unsigned('s', 4,
		&(getopt.UnsignedLimit{0, 28, 1, 1 << 28}),
		`image pixels (type eps[i]: points) per QR module ("pixel"); `+
			`ignored for types utf8[i] and ascii[i]`, "scale")
	ff := getopt.Enum('t', formats, "", `output format, one of: `+
		strings.Join(formats, ", ")+
		`; types with "i" appended have colours inverted; `+
		`"png" uses a fast bespoke QR PNG encoder, more efficient `+
		`at most scales; "PNG" uses the standard Go encoder, `+
		`occasionally more efficient; `+
		`if no -o is given and standard output is a TTY, `+
		`default is utf8, otherwise png`, "type")

	getopt.Parse()
	if g.fnc1flag && getopt.IsSet('C') {
		fmt.Fprintln(os.Stderr, "-c and -C are incompatible")
		usage()
	}
	if g.cformat != 0 {
		for _, v := range "SeEcC" {
			if getopt.IsSet(v) {
				fmt.Fprintf(os.Stderr,
					"-M and -%c are incompatible\n", v)
				usage()
			}
		}
	}
	g.scale = int(*scale)
	g.ver = coding.Version(*ver)
	g.lev = qr.Level(strings.Index("lmqhLMQH", *lev) & 3)
	g.eci = int(*eci)
	if !getopt.IsSet('m') {
		g.border = -1
	}
	if *fnc1 < 256 {
		g.fnc1 = int(*fnc1)
		g.fnc1flag = true
	}
	if *ff == "" {
		if !fno.Seen() && isatty.IsTerminal(uintptr(syscall.Stdout)) {
			*ff = "utf8"
		} else {
			*ff = "png"
		}
	}
	for i, v := range formats {
		if *ff == v {
			g.format = i >> 1
			g.rev = i&1 != 0
			break
		}
	}
	if g.fn == "-" {
		g.fn = ""
	}
	if g.eciflag && !getopt.IsSet('E') {
		switch {
		case g.latin1:
			g.eci = qr.Latin1ECI
		case g.sjis:
			g.eci = qr.ShiftJISECI
		default:
			g.eci = qr.UTF8ECI
		}
	}
	if g.colSet {
		g.palette = &[2]color.Color{color.RGBA(g.bg), color.RGBA(g.fg)}
	}
}

func main() {
	log.SetFlags(0)
	parseFlags()

	var s string
	if args := getopt.Args(); len(args) != 0 {
		s = strings.Join(args, " ")
	} else {
		var b strings.Builder
		if _, err := io.Copy(&b, os.Stdin); err != nil {
			log.Fatalln(err)
		}
		s, _ = strings.CutSuffix(
			strings.ReplaceAll(b.String(), "\r\n", "\n"), "\n")
	}
	if g.upper {
		s = strings.ToUpper(s)
	}

	// Set byte and kanji modes and full charset.  Input:
	//   -k:      Shift JIS input, -KK ignored
	//   -KK:     8 bit input, no conversion to Latin-1
	//   default: UTF-8 input
	// Byte mode encoding, default ECI:
	//   -1:      Latin-1; input: UTF-8, -k: Shift JIS, -KK: no conversion
	//   -k:      Shift JIS, no conversion
	//   default: UTF-8, no conversion; -KK: 8 bit
	bm, km := coding.Byte, coding.Kanji
	var cs split.Charset
	switch {
	case g.latin1 && g.sjis:
		km = coding.ShiftJISKanji
		m := coding.GetMode(coding.Latin1)
		m.Name = "shift-jis-" + m.Name
		m.CutRune, _ = km.RuneFilter()
		m.Accepts = isShiftJISLatin1
		m.Transform = shiftJISToLatin1
		bm = coding.AddMode(m)
	case g.sjis:
		km = coding.ShiftJISKanji
		cs = split.ShiftJIS
		if g.multi {
			m := coding.GetMode(coding.Byte)
			m.Name = "shift-jis"
			m.CutRune, _ = km.RuneFilter() // for -S
			bm = coding.AddMode(m)
		}
	case g.eightBit:
		if g.multi {
			m := coding.GetMode(coding.Byte)
			m.CutRune = func(s string) (rune, int) {
				return rune(s[0]), 1
			}
			bm = coding.AddMode(m)
		}
	case g.latin1:
		bm = coding.Latin1
		cs = split.UTF8AsLatin1
	}
	// Set charset.
	if g.byteOnly && g.multi ||
		bm != coding.Byte && (g.nokanji || cs == nil) {
		// byte mode only (-8); or not plain UTF-8 (-1,-k,-KK), and
		// kanji disabled (-K) or charset not set above (-1k,-KK)
		var ml = split.ModeList{bm,
			split.Disabled, split.Disabled, split.Disabled}
		if !g.byteOnly {
			ml[1] = coding.Numeric
			ml[2] = coding.Alphanumeric
			if !g.nokanji {
				ml[3] = km
			}
		}
		cs = split.NewCharset(ml, 1)
	} else if g.nokanji {
		cs = split.ASCIICompat // UTF-8, kanji disabled
	}

	if g.multi {
		g.fext = path.Ext(g.fn)
		g.fn = g.fn[:len(g.fn)-len(g.fext)]
		cc, err := qr.EncodeTextMulti(s, cs, g.eci, g.ver, g.lev)
		if err != nil {
			log.Fatalln(err)
		}
		for i := range cc {
			write(i, cc[i])
		}
	} else {
		var d split.Data
		if g.byteOnly {
			e := split.ShouldSetECI(g.eci)
			t := split.Segment{s, bm}
			if g.fnc1flag {
				t.Text = strings.ReplaceAll(s, "%", "%%")
				d = split.List{e, split.SetFNC1(g.fnc1), t}
			} else {
				d = split.List{e, t}
			}
		} else if g.fnc1flag {
			d = split.FNC1Text(s, cs, g.eci, g.fnc1)
		} else {
			d = split.Text(s, cs, g.eci)
		}
		c, err := qr.EncodeData(d, g.lev, g.cformat)
		if err != nil {
			log.Fatalln(err)
		}
		write(-1, c)
	}
}

func write(i int, c *qr.Code) {
	fn := g.fn
	open := fn != "" || g.fext != ""
	var w = os.Stdout
	if open {
		if i >= 0 {
			fn = fmt.Sprintf("%s-%02d%s", fn, i+1, g.fext)
		}
		var err error
		if w, err = os.OpenFile(fn, os.O_WRONLY|os.O_CREATE|os.O_TRUNC,
			0666); err != nil {
			log.Fatalln(err)
		}
	}
	c = randr(c)
	c.Scale = g.scale
	c.Palette = g.palette
	c.Reverse = g.rev
	if g.border >= 0 {
		c.Border = g.border
	}
	err := encoders[g.format](c, w)
	if open && err == nil {
		err = w.Close()
	}
	if err != nil {
		log.Fatalln(err)
	}
}

// randr rotates and reflects c.
func randr(c *qr.Code) *qr.Code {
	cx, inc := g.cx, g.inc
	if cx == 0 && inc == [2]int{1, 1} {
		return c
	}
	b := make([]byte, 0, len(c.Bitmap))
	var coord [2]int
	siz := c.Size
	coord[cx^1] = (siz - 1) & inc[1]
	for y := 0; y < siz; y++ {
		coord[cx] = (siz - 1) & inc[0]
		var bb byte
		for x := 0; x < siz; x++ {
			bb <<= 1
			if c.Black(coord[0], coord[1]) {
				bb |= 1
			}
			if x&7 == 7 {
				b = append(b, bb)
			}
			coord[cx] += inc[0]
		}
		b = append(b, bb<<(8-siz&7))
		coord[cx^1] += inc[1]
	}
	c.Bitmap = b
	return c
}

func eps(c *qr.Code, w io.Writer) error {
	const midx, midy = 306, 396
	const maxx, maxy = 612, 792
	siz := c.Size
	scale := c.Scale
	bord := c.Border
	xorig := (midx*2 - (siz+2*bord)*scale) / 2
	yorig := (midy*2 - (siz+2*bord)*scale) / 2
	µ := ""
	if siz < 20 {
		µ = "Micro "
	}
	fmt.Fprintf(w, `%%!PS-Adobe-2.0 EPSF-2.0
%%%%Creator: QR https://github.com/unixdj/qr
%%%%Title: %sQR Code
%%%%BoundingBox: %d %d %d %d
%%%%EndComments
%%%%EndProlog
<< >> begin
gsave
%g %g translate
%d dup neg scale
/row 0 def
/p { 0 rmoveto 0 rlineto } def
/r { 0 row 1 add dup /row exch def moveto } def
`,
		µ, xorig-1, yorig-1, midx*2-xorig, midy*2-yorig,
		midx-float64(siz*scale)/2, midy+float64((siz-1)*scale)/2-1,
		scale)
	if rev := c.Reverse; rev || g.colSet {
		bg, fg := g.bg, g.fg
		if rev {
			bg, fg = fg, bg
			c.Reverse = false
		}
		fmt.Fprintf(w, `gsave
newpath %d %d moveto
%d dup neg scale
%.3g %.3g %.3g setrgbcolor
1 0 rlineto stroke
grestore
%.3g %.3g %.3g setrgbcolor
`,
			-bord, siz/2, siz+2*bord,
			float64(bg.R)/0xff, float64(bg.G)/0xff,
			float64(bg.B)/0xff, float64(fg.R)/0xff,
			float64(fg.G)/0xff, float64(fg.B)/0xff)
	}
	fmt.Fprintln(w, "newpath 0 0 moveto")
	for y := 0; y < siz; y++ {
		for x := 0; x < siz; {
			s := x
			for x < siz && !c.Black(x, y) {
				x++
			}
			if x == siz {
				break
			}
			b := x
			for x < siz && c.Black(x, y) {
				x++
			}
			fmt.Fprintf(w, "%d %d p ", x-b, b-s)
		}
		fmt.Fprintln(w, "r")
	}
	fmt.Fprintln(w, "stroke grestore\nend\n%%Trailer")
	return nil
}

func ascii(c *qr.Code, w io.Writer) error {
	siz := c.Size
	bord := c.Border
	pix := siz + 2*bord
	b := make([]byte, (pix*2+1)*pix)
	i := 0
	for y := -bord; y < siz+bord; y++ {
		for x := -bord; x < siz+bord; x++ {
			var p byte = ' '
			if c.Black(x, y) {
				p = '#'
			}
			_ = b[i+1]
			b[i], b[i+1] = p, p
			i += 2
		}
		b[i] = '\n'
		i++
	}
	_, err := w.Write(b)
	return err
}

var shiftJISLatin1 = [256]byte{
	0x4c: 0xb4, // ´
	0x4e: 0xa8, // ¨
	0x7d: 0xb1, // ±
	0x7e: 0xd7, // ×
	0x80: 0xf7, // ÷
	0x8b: 0xb0, // °
	// 0x8f: 0xa5, // ¥
	// 0x91, 0xa2, // ¢
	// 0x92: 0xa3, // £
	0x98: 0xa7, // §
	// 0xca: 0xac, // ¬
	0xf7: 0xb6, // ¶
}

func isShiftJISLatin1(r rune) bool {
	return r < 0x80 || r>>8 == 0x81 && shiftJISLatin1[byte(r)] != 0
}

func shiftJISToLatin1(s string) (coding.Segment, bool) {
	i := 0
	var b strings.Builder
	for {
		start := i
		for i < len(s) && s[i] < 0x80 {
			i++
		}
		if start == 0 {
			if i == len(s) {
				return coding.Segment{s, coding.Byte}, true
			}
			b.Grow(len(s))
		}
		b.WriteString(s[start:i])
		if i == len(s) {
			return coding.Segment{b.String(), coding.Byte}, true
		}
		if len(s) < i+2 || s[i] != 0x81 {
			break
		}
		c := shiftJISLatin1[s[i+1]]
		if c == 0 {
			break
		}
		b.WriteByte(c)
		i += 2
	}
	return coding.Segment{}, false
}
