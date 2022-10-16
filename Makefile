CALC_PATH := Haskulator

build:
	ghc Main.hs -odir out -hidir out -o $(CALC_PATH)

clean:
	rm -rf out
	rm -f $(CALC_PATH)
