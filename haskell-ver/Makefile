files := Main.hs Ast.hs Display.hs Edit.hs

Main: $(files)
	ghc Main.hs -O3

run: Main
	./Main

run_:
	runhaskell Main.hs

format:
	hindent $(files)
	stylish-haskell -i $(files)

clean:
	-rm *.o *.hi Main

.PHONY: format clean
