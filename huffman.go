// Copyright 2024 Vadim Vygonets.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package qr

// Huffman codes.  Part of the PNG encoder.

import "strconv"

const (
	nsyms   = 286 // literal/length codes
	nlcodes = 29  // length codes
	ndcodes = 30  // distance codes
	nhcodes = 19  // header code length codes
)

// code is a Huffman code or other bit sequence for encoding.
type code struct {
	bit  uint16 // value
	nbit byte   // bit length
}
type ctable []code

type xcode struct {
	bit  uint64 // value
	nbit byte   // bit length
}

// inc returns bit-reversed code c incremented by 1.  c.nbit must be
// less than 16.  On overflow strange things happen.
func (c code) inc() code {
	hi := uint16(1) << c.nbit
	lo := hi >> 1
	for c.bit&lo != 0 {
		lo >>= 1
	}
	c.bit ^= hi - lo
	return c
}

func (c code) add(cc code) code {
	c.bit |= cc.bit << c.nbit
	if c.nbit += cc.nbit; c.nbit > 16 {
		panic("qr: internal error")
	}
	return c
}

func (c code) xadd(cc code) xcode {
	return xcode{uint64(c.bit) | uint64(cc.bit)<<c.nbit, c.nbit + cc.nbit}
}

func (c xcode) add(cc code) xcode {
	if c = c.addUnsafe(cc); c.nbit > 64 {
		panic("qr: internal error")
	}
	return c
}

func (c xcode) addUnsafe(cc code) xcode {
	c.bit |= uint64(cc.bit) << c.nbit
	c.nbit += cc.nbit
	return c
}

func (c code) xcode() xcode { return xcode{uint64(c.bit), c.nbit} }

// code returns the Huffman code for symbol v.
func (c ctable) code(v uint16) code {
	if int(v) < len(c) {
		if cc := c[v]; cc.nbit != 0 {
			return cc
		}
	}
	panic("qr: internal error: flate code " + strconv.Itoa(int(v)))
}

func (c ctable) codex(v uint16, ext code) code   { return c.code(v).add(ext) }
func (c ctable) xcodex(v uint16, ext code) xcode { return c.code(v).xadd(ext) }

/*
When incoding an image using dynamic Huffman codes, two code tables
are built: for literal/length and header code alphabets.  Distance
codes are built manually.

The code table is built in four stages:
  1. Create a linked list of leaves for symbols ordered by freq.
  2. Build the tree.  Until list length is 1, remove two trees from
     the list, merge and insert the result.
  3. If the tree depth exceeds maxdepth, rebalance it.
  4. Build the code table by breadth first traversal.  Codes for each
     depth are sorted by symbol.

Optimisations:
  - Many bytes may have the same frequency (at scale 1 with
    estimateFreqs1, two sets of 240 and 15 bytes).  Set skip for each
    node to &next of the last in a span of trees with the same freq.
    When traversing the list, skip over spans.  When merging, process
    pairs of trees with same freq together, then insert the whole
    span.
  - Save insertion point after traversal for next iteration.  If it
    points into a merged tree after merge, reset to head.
  - At stage 4 sort short symbol lists using insertion sort, for
    longer ones use a bit field instead.

There are several ways to deal with tree depth limit:
  - Tracking tree depth while merging.  Simple enough, but requires
    disabling skip optimisation if maxdepth is exceeded, and gives
    less optimal results.
  - Rebalancing tree after merging.  More optimal, but sounds pretty
    complicated with actual trees.
  - Package merge algorithm.  Probably optimal, but not tried.

With the two approaches that have been tried, a more frequent symbol
may end up deeper in the tree, requiring sorting across depths.  The
solution is to decouple symbol values from trees:
  - After stage 1 save sorted symbols in an array.
  - At stage 3 take symbols for each depth off the array.

Now a tree can be represented by an array of symbol counts at each
depth.
  - To merge trees, add the counts and increment the base depth.
  - If array size is exceeded during merge, limit depth while keeping
    it balanced and note the number of excess symbols.
  - Breadth first traversal and rebalancing are simplified.
*/

// tree represents a Huffman tree.
type tree struct {
	next  *tree      // next tree in linked list
	skip  **tree     // &next of last tree in list with same freq
	freq  int        // sum of symbol frequencies
	syms  [16]uint16 // number of symbols at each depth
	at    byte       // depth offset of syms[0]
	depth byte       // depth below at (0 for leaves)
	sym   uint16     // symbol index (only used before merging)
}

// buildCodes returns a Huffman code table with codes up to maxdepth
// bits from symbol frequences in f, using c for storage.  c must have
// enough capacity for the codes and have zero value.  buildCodes
// clobbers f.
func buildCodes(c ctable, f []int, maxdepth int) ctable {
	var syms, last int
	for i, v := range f {
		if v != 0 {
			syms++
			last = i
		}
	}
	f = f[:last+1]
	c = c[:last+1]
	if syms <= 1 {
		c[last].nbit = byte(syms)
		return c
	}

	// Build linked list of leaves ordered by frequency.
	nodes := make([]tree, syms) // node pool
	idx := 0                    // next empty node
	var head *tree              // linked list
	for i, v := range f {
		if v != 0 {
			// fmt.Printf("node %3d %6d\n", i, v)
			node := &nodes[idx]
			idx++
			node.skip = &node.next
			node.sym = uint16(i)
			node.freq = v
			node.syms[0] = 1
			next := &head
			for *next != nil && (*next).freq < v {
				next = (*next).skip
			}
			if *next != nil && (*next).freq == v {
				node.skip = (*next).skip
			}
			node.next = *next
			*next = node
		}
	}
	// Save ordered symbols.
	f = f[:syms]
	for node := head; node != nil; node = node.next {
		syms--
		f[syms] = int(node.sym)
	}

	// Build Huffman tree.
	var excess int // symbols overflowing tree.syms
	ins := &head   // insertion point
	for head.next != nil {
		freq := head.freq // freq for mass merge
		const depthLimit = byte(len(head.syms) - 1)
		l, r, limit := removeFrom(&head)
		if limit {
			excess += r.limitDepth(l.at + depthLimit)
		}
		node := l.merge(r)
		node.next = nil
		node.skip = &node.next
		// Merge pairs of trees with same freq, add to node list.
		for head != nil && head.next != nil && head.next.freq == freq {
			l, r, limit := removeFrom(&head)
			if limit {
				excess += r.limitDepth(l.at + depthLimit)
			}
			l = l.merge(r)
			l.next = node
			l.skip = node.skip
			node = l
		}
		// If ins points within merged nodes, reset to head.
		if *ins == nil || (*ins).skip == node.skip {
			ins = &head
		}
		// Insert.
		for *ins != nil && (*ins).freq < node.freq {
			ins = (*ins).skip
		}
		*node.skip = *ins
		*ins = node
		if skip := node.skip; *skip != nil {
			ins = skip // ensure *ins!=nil to avoid backtracking
		}
	}

	// Rebalance the tree if it exceeds maxdepth.
	if excess != 0 || head.at+head.depth > byte(maxdepth) {
		// Theory:  In a balanced tree of weight 1, the weight of a
		// leaf at depth n is 1/2**n.  In other words, Huffman code
		// 00 weighs as much as 010, 0110 and 0111 together.  To
		// rebalance the tree:
		//   - Bring all nodes below maxdepth up and note the
		//     excess weight (multiplied by 2**maxdepth).
		//   - Sink some nodes deeper until excess weight is gone.
		//   - If excess weight is negative, bring some nodes up.
		// The second stage is done in a stupid way.
		// fmt.Println(head.syms)
		excess += head.limitDepth(byte(maxdepth))
		md := head.depth
		head.syms[md] += uint16(excess)
		// Prefer sinking deeper symbols, find largest minimum depth.
		d := md
		for sum := 0; sum < excess; {
			d--
			sum += (1<<(md-d) - 1) * int(head.syms[d])
		}
		// fmt.Println(head.syms)
		// Sink symbols deeper until excess is non-positive.
		// Prefer sinking deeper symbols, otherwise avoid sinking
		// a symbol more than once if possible (only looking one
		// level below).  Within its limitations this approach is
		// optimal for excess<=2, which seems to always hold in our
		// use case, otherwise it's probably ok-ish.
		for below := head.syms[d+1] * (1<<(md-d-1) - 1); excess > 0; {
			for head.syms[d] == 0 || excess <= int(below) {
				d++
				below = head.syms[d+1] * (1<<(md-d-1) - 1)
			}
			head.syms[d]--
			head.syms[d+1]++
			excess -= 1 << (md - d - 1)
			// fmt.Println("excess", excess, 1<<(md-d-1))
		}
		// fmt.Println(head.syms)
		// If excess is negative, float some symbols uptree.
		// Prefer shallow symbols.  (This part is quite alright.)
		for d += 2; excess < 0; {
			for -excess < 1<<(md-d) || head.syms[d] == 0 {
				d++
			}
			head.syms[d]--
			head.syms[d-1]++
			excess += 1 << (md - d)
		}
		// fmt.Println(head.syms)
	}

	// Populate code table.
	next := code{nbit: byte(head.at - 1)}
	for _, nsym := range head.syms[:head.depth+1] {
		if next.bit>>next.nbit != 0 {
			panic("qr: unbalanced Huffman tree")
		}
		next.nbit++
		if nsym == 0 {
			continue
		}
		lit := f[:nsym]
		f = f[nsym:]
		// fmt.Println(lit)
		if nsym < 7 {
			// Insertion sort.
			for i := 0; i < len(lit)-1; i++ {
				if v := lit[i+1]; v < lit[i] {
					lit[i+1] = lit[i]
					j := i
					for ; j > 0 && v < lit[j-1]; j-- {
						lit[j] = lit[j-1]
					}
					lit[j] = v
					// fmt.Println(lit)
				}
			}
			for _, v := range lit {
				// fmt.Printf("code %3d %0*b\n", v, next.nbit, next.bit)
				c[v] = next
				next = next.inc()
			}
		} else {
			// Set bit field of symbols.
			var bm [9]uint32
			for _, v := range lit {
				bm[v>>5] |= 1 << (v & 0x1f)
			}
			for i, v := range bm[:len(c)>>5+1] {
				for i <<= 5; v != 0; v &= v - 1 {
					// fmt.Printf("code %3d %0*b\n", i|log2(v&-v), next.nbit, next.bit)
					c[i|log2(v&-v)] = next
					next = next.inc()
				}
			}
		}
	}
	if len(f) != 0 || next.bit>>next.nbit == 0 {
		panic("qr: unbalanced Huffman tree")
	}
	return c
}

// A weird inlined function, don't pay attention.
func removeFrom(head **tree) (*tree, *tree, bool) {
	l, r := *head, (*head).next
	*head = r.next
	if r.at < l.at {
		l, r = r, l
	}
	return l, r, r.at+r.depth >= l.at+byte(len(l.syms))
}

// limitDepth limits depth of t to md-t.at and returns the amount of
// excess symbols.
//
//go:noinline
func (t *tree) limitDepth(md byte) int {
	// See comment in buildCodes about rebalancing for explanation.
	var add, sub uint16
	md -= t.at
	for i := t.depth; i > md; i-- {
		add += t.syms[i]
		sub += t.syms[i]
		sub >>= 1
		t.syms[i] = 0
	}
	t.depth = md
	t.syms[md] += sub
	return int(add - sub)
}

// merge merges t with u.
func (t *tree) merge(u *tree) *tree {
	diff := u.at - t.at
	t.depth = max(t.depth, u.depth+diff)
	ls := t.syms[diff : t.depth+1]
	for i := range ls {
		ls[i] += u.syms[i]
	}
	t.at++
	t.freq += u.freq
	u.next = nil // for ins reset detection
	return t
}

// dynamic header length code
type lencode struct {
	cmd uint16 // command
	arg code   // argument
}

// lenCodes returns header codes for constructing code lengths for c.
func lenCodes(c []code) []lencode {
	d := make([]lencode, 0, 64) // enough for all scales except 1
	for i := 0; i < len(c); {
		start, v := i, c[i].nbit
		// if length is not 0, add code
		if v != 0 {
			d = append(d, lencode{cmd: uint16(v)})
			start++
		}
		// count repeats
		for i++; i < len(c) && c[i].nbit == v; i++ {
		}
		if r := uint16(i - start); r < 3 {
			// 0-2 repeats.  add code.
			for ; r > 0; r-- {
				d = append(d, lencode{cmd: uint16(v)})
			}
		} else if v == 0 {
			// length is 0.  add zeroes (codes 17 and 18).
			for r > 140 {
				d = append(d, lencode{18, code{0x7f, 7}})
				r -= 138
			}
			if r > 138 {
				d = append(d, lencode{18, code{r - 3 - 11, 7}},
					lencode{17, code{0, 3}})
			} else if r > 10 {
				d = append(d, lencode{18, code{r - 11, 7}})
			} else {
				d = append(d, lencode{17, code{r - 3, 3}})
			}
		} else {
			// add repeats (code 16).
			for r > 8 {
				d = append(d, lencode{16, code{3, 2}})
				r -= 6
			}
			if r > 6 {
				d = append(d, lencode{16, code{0, 2}})
				r -= 3
			}
			d = append(d, lencode{16, code{r - 3, 2}})
		}
	}
	return d
}
