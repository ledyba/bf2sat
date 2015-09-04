.PHONY: hello simple simple-nondet prime-fact clean

hello:
	cabal run create hello.bf
	minisat sat.txt ans.txt || true
	cabal run decode hello.bf

simple:
	cabal run create simple.bf
	minisat sat.txt ans.txt || true
	cabal run decode simple.bf

simple-nondet:
	cabal run create simple-nondet.bf
	minisat sat.txt ans.txt || true
	cabal run decode simple-nondet.bf

prime-fact:
	cabal run create mult.bf
	minisat sat.txt ans.txt || true
	cabal run decode mult.bf

clean:
	rm sat.txt pred.txt ans.txt
