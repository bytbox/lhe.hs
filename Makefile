all: demo

demo: Main.hs Text/LHE.hs
	ghc -o $@ Main.hs
