//go:build ignore

package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"
)

// tables from qrencode-3.1.1/qrspec.c

var capacity = [45]struct {
	width     int
	words     int
	remainder int
	ec        [4]int
}{
	{0, 0, 0, [4]int{0, 0, 0, 0}},
	{21, 26, 0, [4]int{7, 10, 13, 17}}, // 1
	{25, 44, 7, [4]int{10, 16, 22, 28}},
	{29, 70, 7, [4]int{15, 26, 36, 44}},
	{33, 100, 7, [4]int{20, 36, 52, 64}},
	{37, 134, 7, [4]int{26, 48, 72, 88}}, // 5
	{41, 172, 7, [4]int{36, 64, 96, 112}},
	{45, 196, 0, [4]int{40, 72, 108, 130}},
	{49, 242, 0, [4]int{48, 88, 132, 156}},
	{53, 292, 0, [4]int{60, 110, 160, 192}},
	{57, 346, 0, [4]int{72, 130, 192, 224}}, //10
	{61, 404, 0, [4]int{80, 150, 224, 264}},
	{65, 466, 0, [4]int{96, 176, 260, 308}},
	{69, 532, 0, [4]int{104, 198, 288, 352}},
	{73, 581, 3, [4]int{120, 216, 320, 384}},
	{77, 655, 3, [4]int{132, 240, 360, 432}}, //15
	{81, 733, 3, [4]int{144, 280, 408, 480}},
	{85, 815, 3, [4]int{168, 308, 448, 532}},
	{89, 901, 3, [4]int{180, 338, 504, 588}},
	{93, 991, 3, [4]int{196, 364, 546, 650}},
	{97, 1085, 3, [4]int{224, 416, 600, 700}}, //20
	{101, 1156, 4, [4]int{224, 442, 644, 750}},
	{105, 1258, 4, [4]int{252, 476, 690, 816}},
	{109, 1364, 4, [4]int{270, 504, 750, 900}},
	{113, 1474, 4, [4]int{300, 560, 810, 960}},
	{117, 1588, 4, [4]int{312, 588, 870, 1050}}, //25
	{121, 1706, 4, [4]int{336, 644, 952, 1110}},
	{125, 1828, 4, [4]int{360, 700, 1020, 1200}},
	{129, 1921, 3, [4]int{390, 728, 1050, 1260}},
	{133, 2051, 3, [4]int{420, 784, 1140, 1350}},
	{137, 2185, 3, [4]int{450, 812, 1200, 1440}}, //30
	{141, 2323, 3, [4]int{480, 868, 1290, 1530}},
	{145, 2465, 3, [4]int{510, 924, 1350, 1620}},
	{149, 2611, 3, [4]int{540, 980, 1440, 1710}},
	{153, 2761, 3, [4]int{570, 1036, 1530, 1800}},
	{157, 2876, 0, [4]int{570, 1064, 1590, 1890}}, //35
	{161, 3034, 0, [4]int{600, 1120, 1680, 1980}},
	{165, 3196, 0, [4]int{630, 1204, 1770, 2100}},
	{169, 3362, 0, [4]int{660, 1260, 1860, 2220}},
	{173, 3532, 0, [4]int{720, 1316, 1950, 2310}},
	{177, 3706, 0, [4]int{750, 1372, 2040, 2430}}, //40
	{11, 5, 0, [4]int{2, 5, 5, 5}},                // M1
	{13, 10, 0, [4]int{5, 6, 10, 10}},             // M2
	{15, 17, 0, [4]int{6, 8, 17, 17}},             // M3
	{17, 24, 0, [4]int{8, 10, 14, 24}},            // M4
}

var eccTable = [45][4][2]int{
	{{0, 0}, {0, 0}, {0, 0}, {0, 0}},
	{{1, 0}, {1, 0}, {1, 0}, {1, 0}}, // 1
	{{1, 0}, {1, 0}, {1, 0}, {1, 0}},
	{{1, 0}, {1, 0}, {2, 0}, {2, 0}},
	{{1, 0}, {2, 0}, {2, 0}, {4, 0}},
	{{1, 0}, {2, 0}, {2, 2}, {2, 2}}, // 5
	{{2, 0}, {4, 0}, {4, 0}, {4, 0}},
	{{2, 0}, {4, 0}, {2, 4}, {4, 1}},
	{{2, 0}, {2, 2}, {4, 2}, {4, 2}},
	{{2, 0}, {3, 2}, {4, 4}, {4, 4}},
	{{2, 2}, {4, 1}, {6, 2}, {6, 2}}, //10
	{{4, 0}, {1, 4}, {4, 4}, {3, 8}},
	{{2, 2}, {6, 2}, {4, 6}, {7, 4}},
	{{4, 0}, {8, 1}, {8, 4}, {12, 4}},
	{{3, 1}, {4, 5}, {11, 5}, {11, 5}},
	{{5, 1}, {5, 5}, {5, 7}, {11, 7}}, //15
	{{5, 1}, {7, 3}, {15, 2}, {3, 13}},
	{{1, 5}, {10, 1}, {1, 15}, {2, 17}},
	{{5, 1}, {9, 4}, {17, 1}, {2, 19}},
	{{3, 4}, {3, 11}, {17, 4}, {9, 16}},
	{{3, 5}, {3, 13}, {15, 5}, {15, 10}}, //20
	{{4, 4}, {17, 0}, {17, 6}, {19, 6}},
	{{2, 7}, {17, 0}, {7, 16}, {34, 0}},
	{{4, 5}, {4, 14}, {11, 14}, {16, 14}},
	{{6, 4}, {6, 14}, {11, 16}, {30, 2}},
	{{8, 4}, {8, 13}, {7, 22}, {22, 13}}, //25
	{{10, 2}, {19, 4}, {28, 6}, {33, 4}},
	{{8, 4}, {22, 3}, {8, 26}, {12, 28}},
	{{3, 10}, {3, 23}, {4, 31}, {11, 31}},
	{{7, 7}, {21, 7}, {1, 37}, {19, 26}},
	{{5, 10}, {19, 10}, {15, 25}, {23, 25}}, //30
	{{13, 3}, {2, 29}, {42, 1}, {23, 28}},
	{{17, 0}, {10, 23}, {10, 35}, {19, 35}},
	{{17, 1}, {14, 21}, {29, 19}, {11, 46}},
	{{13, 6}, {14, 23}, {44, 7}, {59, 1}},
	{{12, 7}, {12, 26}, {39, 14}, {22, 41}}, //35
	{{6, 14}, {6, 34}, {46, 10}, {2, 64}},
	{{17, 4}, {29, 14}, {49, 10}, {24, 46}},
	{{4, 18}, {13, 32}, {48, 14}, {42, 32}},
	{{20, 4}, {40, 7}, {43, 22}, {10, 67}},
	{{19, 6}, {18, 31}, {34, 34}, {20, 61}}, //40
	{{1, 0}, {1, 0}, {1, 0}, {1, 0}},        // M1
	{{1, 0}, {1, 0}, {1, 0}, {1, 0}},        // M2
	{{1, 0}, {1, 0}, {1, 0}, {1, 0}},        // M3
	{{1, 0}, {1, 0}, {1, 0}, {1, 0}},        // M4
}

var align = [45][2]int{
	{0, 0},
	{0, 0}, {18, 0}, {22, 0}, {26, 0}, {30, 0}, // 1- 5
	{34, 0}, {22, 38}, {24, 42}, {26, 46}, {28, 50}, // 6-10
	{30, 54}, {32, 58}, {34, 62}, {26, 46}, {26, 48}, //11-15
	{26, 50}, {30, 54}, {30, 56}, {30, 58}, {34, 62}, //16-20
	{28, 50}, {26, 50}, {30, 54}, {28, 54}, {32, 58}, //21-25
	{30, 58}, {34, 62}, {26, 50}, {30, 54}, {26, 52}, //26-30
	{30, 56}, {34, 60}, {30, 58}, {34, 62}, {30, 54}, //31-35
	{24, 50}, {28, 54}, {32, 58}, {26, 54}, {30, 58}, //35-40
	{0, 0}, {0, 0}, {0, 0}, {0, 0}, // M1-M4
}

var versionPattern = [45]int{
	0,
	0, 0, 0, 0, 0, 0,
	0x07c94, 0x085bc, 0x09a99, 0x0a4d3, 0x0bbf6, 0x0c762, 0x0d847, 0x0e60d,
	0x0f928, 0x10b78, 0x1145d, 0x12a17, 0x13532, 0x149a6, 0x15683, 0x168c9,
	0x177ec, 0x18ec4, 0x191e1, 0x1afab, 0x1b08e, 0x1cc1a, 0x1d33f, 0x1ed75,
	0x1f250, 0x209d5, 0x216f0, 0x228ba, 0x2379f, 0x24b0b, 0x2542e, 0x26a64,
	0x27541, 0x28c69,
	0, 0, 0, 0,
}

func calcFormat(fb uint16) uint16 {
	const formatPoly = 0x537
	rem := fb
	for i := 4; i >= 0; i-- {
		if rem&((1<<10)<<i) != 0 {
			rem ^= formatPoly << i
		}
	}
	return fb | rem
}

const (
	url = "http://encoding.spec.whatwg.org/index-jis0208.txt"

	// Maximum encodable kanji is:
	//   ku, ten:    86, 33
	//   Shift-JIS:  hi = (ku+0x181)/2 = 0xeb  lo = ten+0x9e = 0xbf
	//   QR kanji:   (hi&^0xc0-1)*c0 + (lo-0x40) = 0x1fff
	//   index:      (ku-1)*94 + (ten-1) = 8022
	maxQR    = 0x1fff
	maxIndex = maxQR - maxQR/0xc0*4 - (maxQR>>6%3+1)>>1
)

func makeTable(w io.Writer, r io.Reader) {
	var mapping [1 << 11]uint32
	scanner := bufio.NewScanner(r)
	for scanner.Scan() {
		s := strings.TrimSpace(scanner.Text())
		if s == "" || s[0] == '#' {
			continue
		}
		f := strings.Fields(s)
		if len(f) < 2 {
			continue
		}
		k, err := strconv.ParseUint(f[0], 0, 16)
		if err != nil || k > maxIndex {
			continue
		}
		n, err := strconv.ParseUint(f[1], 0, 16)
		if err != nil || n >= 0x10000 {
			continue
		}
		mapping[n/32] |= 1 << (n & 31)
	}
	if scanner.Err() != nil {
		log.Fatalln(scanner.Err())
	}
	fmt.Fprint(w, `
// Bit field of Unicode code points for JIS X 0208 characters
// up to ku-ten 86-33.
var jis0208qr = [2048]uint32{
`)
	i := 0
	for {
		for i < len(mapping) && mapping[i] == 0 {
			i++
		}
		if i == len(mapping) {
			break
		}
		fmt.Fprintf(w, "\t%#04x >> 5:", i<<5)
		for {
			fmt.Fprintf(w, " %#08x,", mapping[i])
			if i++; i&3 == 0 || mapping[i] == 0 {
				break
			}
		}
		fmt.Fprintln(w)
	}
	fmt.Fprintln(w, "}")
}

func main() {
	w := bufio.NewWriter(os.Stdout)
	fmt.Fprint(w, `// generated by go run gen.go | gofmt; DO NOT EDIT

package coding

// Version table.
var vtab = [45]version{
`)
	for i := 1; i <= 44; i++ {
		apos := align[i][0] - 2
		if apos < 0 {
			apos = 100
		}
		astride := align[i][1] - align[i][0]
		if astride < 1 {
			astride = 100
		}
		var vs string
		if i <= 40 {
			vs = fmt.Sprintf("%d", i)
		} else {
			vs = fmt.Sprintf("M%d", i-40)
		}
		var hpat, vpat uint32
		if v := uint32(versionPattern[i]); v != 0 {
			for j := 0; j < 18; j++ {
				vpat = vpat<<1 | v&1
				v >>= 1
			}
			for j, v := 0, vpat; j < 6; j++ {
				hpat >>= 1
				hpat |= v & 7 * 0x8420 & 0404040
				v >>= 3
			}
		}
		fmt.Fprintf(w, "\t%s: {%v, %v, %v, %#x, %#x, [4]level{{%v, %v}, {%v, %v}, {%v, %v}, {%v, %v}}},\n",
			vs,
			apos, astride, capacity[i].words,
			hpat, vpat,
			eccTable[i][0][0]+eccTable[i][0][1],
			capacity[i].ec[0]/(eccTable[i][0][0]+eccTable[i][0][1]),
			eccTable[i][1][0]+eccTable[i][1][1],
			capacity[i].ec[1]/(eccTable[i][1][0]+eccTable[i][1][1]),
			eccTable[i][2][0]+eccTable[i][2][1],
			capacity[i].ec[2]/(eccTable[i][2][0]+eccTable[i][2][1]),
			eccTable[i][3][0]+eccTable[i][3][1],
			capacity[i].ec[3]/(eccTable[i][3][0]+eccTable[i][3][1]),
		)
	}
	fmt.Fprintln(w, "}")

	fmt.Fprint(w, "\n// QR Code format bits.\nvar ftab = [4][8]uint16{\n")
	for l := 0; l < 4; l++ {
		fmt.Fprintf(w, "\t%c: {", "LMQH"[l])
		for m := 0; m < 8; m++ {
			fb := uint16(l^1) << 13 // L=01, M=00, Q=11, H=10
			fb |= uint16(m) << 10   // mask
			fb = calcFormat(fb) ^ 0x5412
			fmt.Fprintf(w, "%#04x, ", fb)
		}
		fmt.Fprintln(w, "},")
	}
	fmt.Fprintln(w, "}")

	fmt.Fprint(w, "\n// Micro QR Code format bits.\n"+
		"var mftab = [8][4]uint16{\n")
	for i := 0; i < 8; i++ {
		fmt.Fprint(w, "\t{")
		for m := 0; m < 4; m++ {
			fb := uint16(i) << 12 // 000-111
			fb |= uint16(m) << 10 // mask
			fb = calcFormat(fb) ^ 0x4445
			fmt.Fprintf(w, "%#04x, ", fb)
		}
		v := min((i+1)/2, 3)
		l := i - max(v*2-1, 0)
		fmt.Fprintf(w, "}, // M%d %c\n", v+1, "LMQH"[l])
	}
	fmt.Fprintln(w, "}")

	res, err := http.Get(url)
	if err != nil {
		log.Fatalf("%q: Get: %v", url, err)
	}
	defer res.Body.Close()
	makeTable(w, res.Body)
	w.Flush()
}
