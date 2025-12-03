default:
	runhaskell Main.hs

faster:
	ghc -O3 Main.hs
	rm Main.o
	rm Main.hi
	./Main
	rm Main

