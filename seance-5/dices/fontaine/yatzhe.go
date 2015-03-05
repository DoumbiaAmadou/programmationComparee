package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"os"
	"sort"
)

const (
	minDie = Die(1)
	maxDie = Die(6)

	dicesCount = 5
)

var (
	errUnknownHand = errors.New("Unknown hand")
)

// Die represents a dice
type Die int

// Dice is an ordered set of dices
type Dice [dicesCount]Die

func (d *Dice) Len() int           { return len(d) }
func (d *Dice) Swap(i, j int)      { d[i], d[j] = d[j], d[i] }
func (d *Dice) Less(i, j int) bool { return d[i] < d[j] }

// Clone returns a new copy of this set
func (d Dice) Clone() Dice {
	return d
}

// Reverse reverses a dices set in-place
func (d Dice) Reverse() (d2 Dice) {
	for i := range d {
		j := dicesCount - i - 1
		d2[j] = d[i]
	}

	return
}

// A Stream represents a possibly infinite stream of dices sets.
type Stream interface {
	Filter(func(Dice) bool)
	Next() Dice
}

func (d Dice) String() string {
	var s bytes.Buffer

	for _, v := range d {
		s.WriteString(fmt.Sprintf("%d ", v))
	}

	return s.String()
}

// A HandStream is the basic implementation of a Stream
type HandStream struct {
	filters []func(Dice) bool
	dices   Dice
}

// NewHandStream returns a new HandStream
func NewHandStream() *HandStream {
	hs := &HandStream{
		filters: make([]func(Dice) bool, 1),
	}

	for i := range hs.dices {
		hs.dices[i] = minDie
	}

	return hs
}

// Filter adds a new filter to the current stream
func (hs *HandStream) Filter(fn func(Dice) bool) {
	hs.filters = append(hs.filters, fn)
}

func (hs *HandStream) accept() bool {
	for _, fn := range hs.filters {
		if fn == nil {
			continue
		}

		dices := hs.dices.Clone()
		sort.Sort(&dices)

		if !fn(dices) && !fn(dices.Reverse()) {
			return false
		}
	}

	return true
}

// Next returns the next dices set
func (hs *HandStream) Next() *Dice {

	for {

		var i int

		for ; i < dicesCount && hs.dices[i] == maxDie; i++ {
			hs.dices[i] = minDie
		}

		if i == dicesCount {
			return nil
		}

		hs.dices[i]++

		if hs.accept() {
			return &hs.dices
		}

	}
}

// Curr returns the current dices set, if it matches the filter(s). If it
// doesn't, the first matching dices set is returned.
func (hs *HandStream) Curr() *Dice {
	if hs.accept() {
		return &hs.dices
	}

	return hs.Next()
}

// Produce gets a chan, write all dices sets on it and exits
func (hs *HandStream) Produce(d chan Dice) {
	for dices := hs.Curr(); dices != nil; dices = hs.Next() {
		d <- *dices
	}

	os.Exit(0)
}

var filters = map[string]func(Dice) bool{
	"full": func(d Dice) bool {
		return d[0] == d[1] && d[1] == d[2] && d[3] == d[4]
	},
	"suit": func(d Dice) bool {
		step := d[0] - d[1]

		if step != 1 && step != -1 {
			return false
		}

		for i := 1; i+1 < dicesCount; i++ {
			if (d[i] - d[i+1]) != step {
				return false
			}
		}
		return true
	},
	"none": func(d Dice) bool { return true },
}

func printHands(hand string) (err error) {
	fn, ok := filters[hand]

	if !ok {
		err = errUnknownHand
		return
	}

	hs := NewHandStream()
	hs.Filter(fn)

	feed := make(chan Dice, 10)

	go hs.Produce(feed)

	for {
		d, ok := <-feed

		if !ok {
			break
		}

		fmt.Printf("%s\n", d)
	}

	return
}

func main() {
	hand := flag.String("hand", "full", "The hands to produce")
	list := flag.Bool("hands", false, "Print all hands")
	flag.Parse()

	if *list {
		fmt.Println("Possible hands:")
		for k := range filters {
			fmt.Printf("- %s\n", k)
		}

		return
	}

	printHands(*hand)
}
