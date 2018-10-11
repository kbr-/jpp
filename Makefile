all:
	cd bnfc; make
	ghc --make -ibnfc/ Run.hs -o interpreter

clean:
	rm -f *.o *.hi interpreter
