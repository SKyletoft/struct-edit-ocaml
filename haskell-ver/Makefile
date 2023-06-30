files := Main.hs Ast.hs Display.hs Highlight.hs Edit.hs Ui.hs Navigate.hs PrettyDisplay.hs
HS_FLAGS := \
	-XGHC2021 \
	-XOverloadedStrings \
	-XPatternSynonyms \
	-XLambdaCase

Main: $(files) termops.o
	ghc Main.hs $(HS_FLAGS) -O0 -threaded -g termops.o

termops.o: FFI/termops.c
	gcc -c -O -o $@ $^

run: Main
	./Main

run_:
	runhaskell Main.hs $(HS_FLAGS)

format:
	hindent $(files)
	stylish-haskell -i $(files)

clean:
	-rm *.o *.hi *.so Main
	-rm FFI/*.o FFI/*.hi

.PHONY: format clean
