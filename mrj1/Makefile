all:
	happy -gca ParInstant.y
	alex -g LexInstant.x
	ghc -cpp -DJVM --make TestInstant.hs -o JVMCompiler
	ghc -cpp -DLLVM --make TestInstant.hs -o LLVMCompiler
	

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* ComposOp.* Instant.dtd XMLInstant.* Makefile*
	

