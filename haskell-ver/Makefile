files := Main.hs Ast.hs Display.hs Edit.hs
HS_FLAGS := \
	-XGHC2021 \
	-XOverloadedStrings \
	-XPatternSynonyms \
	-XLambdaCase

Main: $(files)
	ghc Main.hs $(HS_FLAGS) -O0 -threaded -g

run: Main
	./Main

run_:
	runhaskell Main.hs $(HS_FLAGS)

format:
	hindent Ast.hs Display.hs Edit.hs
	stylish-haskell -i Ast.hs Display.hs Edit.hs

clean:
	-rm *.o *.hi Main

.PHONY: format clean
