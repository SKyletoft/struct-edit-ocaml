Main: Main.hs Ast.hs Display.hs
	ghc Main.hs -O3

run: Main
	./Main

run_:
	runhaskell Main.hs

clean:
	-rm *.o *.hi Main
