all: demo

demo: Main.hs LHE.hs
	ghc -o $@ Main.hs
