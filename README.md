[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/X15MPpfH)

# Trabalho Prático de BCC328 — Construção de Compiladores I

## Compilador para a Linguagem SL (Simple Language) — Etapa 1

Este repositório contém a implementação da **Etapa 1** do compilador para a linguagem **SL**. O projeto foi desenvolvido em **Haskell** utilizando as ferramentas **Alex** (Lexer) e **Happy** (Parser).

Esta etapa cobre:

* Análise Léxica
* Análise Sintática (com geração de **Árvore Sintática Abstrata — AST**)
* **Pretty Printing**

---

## 🐳 Instruções (Docker)

### Iniciando o container

Execute os comandos abaixo para subir o ambiente:

```bash
docker-compose up -d
docker-compose exec sl bash
```

Dentro do container, você estará em um ambiente pronto para compilar e testar o projeto.

---

## 🛠️ Compilação e Execução

O projeto utiliza o **Cabal** para gerenciamento de dependências e build.

### 1. Compilar o projeto

Dentro do container, execute:

```bash
cabal update
cabal build
```

### 2. Executando o compilador

O compilador aceita três flags principais via linha de comando, conforme a especificação do trabalho:

* `--lexer`: Exibe a lista de tokens reconhecidos com linha e coluna.
* `--parser`: Exibe a **Árvore Sintática Abstrata (AST)** em formato visual hierárquico (utilizando `Data.Tree`).
* `--pretty`: Reconstrói o código fonte a partir da AST (**Pretty Printing**).

#### Exemplos de uso

```bash
# Testar o Lexer (Tokens)
cabal run sl-compiler -- --lexer src/tests/test4_generics.sl

# Visualizar a AST (Árvore Hierárquica)
cabal run sl-compiler -- --parser src/tests/test4_generics.sl

# Testar o Pretty Printer (Formatação de Código)
cabal run sl-compiler -- --pretty src/tests/test4_generics.sl
```

---

## 🧪 Testes Automatizados

O projeto inclui um **Makefile** para facilitar a execução de testes em lote. Foram criados diversos arquivos de teste na pasta `src/tests/`, cobrindo funcionalidades como **Generics**, **Structs**, **Arrays** e **Comentários Aninhados**.

Para rodar todos os testes de parsing sequencialmente:

```bash
make test
```

### Arquivos de teste disponíveis

* `test1.sl`: Controle de fluxo básico e aritmética.
* `test2_structs.sl`: Definição e acesso a campos de Structs.
* `test3_arrays.sl`: Declaração, inicialização e acesso a Arrays.
* `test4_generics.sl`: Funções genéricas (`forall`) e tipos de função.
* `test5_comments.sl`: Testes básicos de comentários.
* `test6_multcomm.sl`: Teste de estresse de comentários de bloco aninhados (`/* ... /* ... */ ... */`).

---

## 📂 Estrutura do Projeto

* `app/Main.hs`: Ponto de entrada do executável. Processa os argumentos da CLI.
* `src/`: Código fonte da biblioteca do compilador.

  * `Lexer.x`: Especificação do analisador léxico (Alex). Implementa suporte a comentários aninhados via `monadUserState`.
  * `Parser.y`: Especificação da gramática e analisador sintático (Happy). Resolve conflitos de precedência.
  * `Tokens.hs`: Definição dos tokens e tipos de dados auxiliares.
  * `AST.hs`: Definição da Árvore de Sintaxe Abstrata (`Types`, `Expr`, `Stmt`, `TopDecl`).
  * `Pretty.hs`: Implementação do Pretty Printer usando a biblioteca `pretty`.
  * `TreeUtils.hs`: Utilitário para converter a AST proprietária em `Data.Tree` para visualização ASCII.
* `src/tests/`: Arquivos de código fonte SL para teste.

---

## ✨ Funcionalidades Implementadas

* [x] **Análise Léxica Completa**: Suporte a todos os tokens da especificação.
* [x] **Comentários Aninhados**: Tratamento robusto de blocos de comentário dentro de outros blocos.
* [x] **Tipos Complexos**: Suporte a Arrays, Structs e Tipos de Função (ex.: `(int) -> bool`).
* [x] **Generics**: Suporte à palavra-chave `forall` e variáveis de tipo.
* [x] **Açúcar Sintático**: Desaçucaramento automático de `i++` e inicialização de arrays durante o parsing.
* [x] **Visualização de Árvore**: Saída legível e identada da AST usando `drawTree`.

---

## 👥 Autores

* Gustavo Zacarias de Souza
* Luiz Eduardo Fugliaro
