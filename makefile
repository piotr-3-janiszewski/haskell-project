all:
	ghc main.hs -O2 -o checkreg

clean:
	rm checkreg *.o *.hi

test: all
	pytest
