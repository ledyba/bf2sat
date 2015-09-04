.PHONY: hello

hello:
	cabal run create hello.bf
	minisat sat.txt ans.txt
	cabal run decode hello.bf

easy:
	cabal run create easy.bf
	minisat sat.txt ans.txt
	cabal run decode easy.bf

easy-nondet:
	cabal run create easy-nondet.bf
	minisat sat.txt ans.txt
	cabal run decode easy-nondet.bf

prime-fact:
	cabal run create mult.bf
	minisat sat.txt ans.txt
	cabal run decode mult.bf

clean:
	rm sat.txt pred.txt ans.txt
