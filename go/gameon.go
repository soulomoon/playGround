package main

var c = make(chan int)
var a string

func f() {
	a = "hello, world\n"
	<-c
}

func main() {
	go f()
	c <- 0
	print(a)
}
