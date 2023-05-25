Main:
	make -C haskell-ver Main

run:
	make -C haskell-ver run

run_:
	make -C haskell-ver run_

format:
	make -C haskell-ver format

clean:
	-make -C haskell-ver clean
	-rm result
	-rm ocaml-ver/_build -rf
