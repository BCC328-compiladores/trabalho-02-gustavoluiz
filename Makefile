.PHONY: test

test:
	cabal run sl-compiler -- --check src\tests\test_sucesso_completo.sl
	cabal run sl-compiler -- --check src\tests\test_erro_tipos.sl
	cabal run sl-compiler -- --check src\tests\test_erro_funcao.sl
	cabal run sl-compiler -- --check src\tests\test_erro_escopo.sl
	cabal run sl-compiler -- --check src\tests\test_erro_condicao.sl
	@echo "✅ Todos os testes passaram com sucesso!"
