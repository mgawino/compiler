all:
	bnfc -haskell src/Latte.cf
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc --make src/LatteCompiler.hs -isrc -o latc_llvm
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi src/*.hi src/*.o
	-rm -f DocLatte.ps Makefile~
distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* Latte.dtd XMLLatte.* latc_llvm latc
