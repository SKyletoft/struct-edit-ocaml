files := Main.hs Ast.hs Display.hs Edit.hs
HS_FLAGS := \
	-XGHC2021 \
	-XOverloadedStrings \
	-XPatternSynonyms \
	-XLambdaCase

Main: $(files)
	ghc Main.hs $(HS_FLAGS) -O0 -threaded

run: Main
	./Main

run_:
	runhaskell Main.hs $(HS_FLAGS)

format:
	hindent $(files)
	stylish-haskell -i $(files)

clean:
	-rm *.o *.hi Main

.PHONY: format clean
