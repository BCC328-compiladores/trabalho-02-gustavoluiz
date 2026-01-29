.PHONY: test

test:
	cabal run sl-compiler -- --parser src/tests/test1.sl
	cabal run sl-compiler -- --parser src/tests/test2_structs.sl
	cabal run sl-compiler -- --parser src/tests/test3_arrays.sl
	cabal run sl-compiler -- --parser src/tests/test4_generics.sl
	cabal run sl-compiler -- --parser src/tests/test5_comments.sl
	cabal run sl-compiler -- --parser src/tests/test6_multcomm.sl
	@echo "✅ Todos os testes passaram com sucesso!"
