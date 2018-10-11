all:
	bnfc -haskell haml.cf
	happy -gca ParHaml.y
	alex -g LexHaml.x
	ghc --make TestHaml.hs -o TestHaml
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Dochaml.ps
distclean: clean
	-rm -f DocHaml.* LexHaml.* ParHaml.* LayoutHaml.* SkelHaml.* PrintHaml.* TestHaml.* AbsHaml.* TestHaml ErrM.* SharedString.* haml.dtd XMLhaml.*

