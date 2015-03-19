package main

import (
	"fmt"
	"math/big"
	"math/rand"
	"os"
	"os/signal"
	"time"
)

func seed() int64 {
	return time.Now().Unix()
}

type Worker struct {
	kChan, nChan chan int64
}

func NewWorker() *Worker {
	return &Worker{
		kChan: make(chan int64, 100),
		nChan: make(chan int64, 100),
	}
}

func (w *Worker) Start() {
	var n, k int64

	r := rand.New(rand.NewSource(seed()))

	for {
		x, y := r.Float64(), r.Float64()

		n++

		if x*x+y*y <= 1 {
			k++
		}

		if n > 1000000 {
			w.kChan <- k
			w.nChan <- n
			n = 0
			k = 0
		}
	}

	return
}

type WorkersPool struct {
	workers []*Worker
}

func NewWorkersPool(capacity int) *WorkersPool {
	wp := &WorkersPool{
		workers: make([]*Worker, capacity),
	}

	for i := range wp.workers {
		wp.workers[i] = NewWorker()
	}

	return wp
}

func (wp *WorkersPool) Start() {
	for _, w := range wp.workers {
		go w.Start()
	}
}

func (wp *WorkersPool) Collect(n, k *int64) {
	for _, w := range wp.workers {
		*n += <-w.nChan
		*k += <-w.kChan
	}
}

func restoreCursor() {
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, os.Kill)
	<-c
	fmt.Print("\033[?25h\n\n\n\n")
	os.Exit(0)
}

func initShow()  { fmt.Printf("\0337") }
func resetShow() { fmt.Printf("\0338") }

func show(pi, n string) {

	initShow()

	goodDigits := 0
	expected := "3.1415926535"

	for i, l1, l2 := 0, len(pi), len(expected); i < l1 && i < l2; i++ {
		if pi[i] != expected[i] {
			break
		}
		goodDigits++
	}

	repr := fmt.Sprintf("\033[4m%s\033[0m%s",
		expected[:goodDigits], pi[goodDigits:])

	fmt.Printf("Pi: %s\n", repr)
	fmt.Printf("\nCorrect decimals: %d  \n", goodDigits-2)
	fmt.Printf("N: %s\n", n)

	resetShow()
}

func main() {
	var n, k int64

	wp := NewWorkersPool(15)
	wp.Start()

	rat4 := big.NewRat(4, 1)
	pi4 := big.NewRat(0, 1)

	N := big.NewInt(0)

	// hide the cursor
	fmt.Print("\033[?25l")

	// hacky way to ensure we restore the cursor at the end
	go restoreCursor()

	for {
		wp.Collect(&n, &k)

		if n >= 100000000 {
			a, b := pi4.Num(), pi4.Denom()

			pi4.SetFrac(
				a.Add(a, big.NewInt(k)),
				b.Add(b, big.NewInt(n)),
			)

			N.Add(N, big.NewInt(n))

			pi := big.NewRat(0, 1)
			show(pi.Mul(pi4, rat4).FloatString(40), N.String())

			n = 0
			k = 0
		}
	}

}
