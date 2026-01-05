all:
	ghc main.hs -O2 -o checkreg

clean:
	rm checkreg *.o *.hi

test: all
	python3 -m pytest
