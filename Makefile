all: demo

demo: Main.hs Data/LHE.hs
	ghc -o $@ Main.hs
