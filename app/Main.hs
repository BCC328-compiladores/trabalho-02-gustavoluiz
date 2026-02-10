module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, openFile, stdout, utf8)

-- Importações do Compilador
import Lexer (runLexer)
import Parser (parse)
import Pretty (prettyPrint)

-- Importações para Desenho da Árvore
import Data.Tree (drawTree)
import TreeUtils (astToTree)

-- Importação do Analisador Semântico
import TypeChecker (runTypeCheck)

-- Importação do Interpretador
import Interpreter (interpret)

-- Importação do Gerador de Código 
import CodeGen (genProgram)

-- A função principal
main :: IO ()
main = do
    hSetEncoding stdout utf8
    
    args <- getArgs
    case args of
        -- MODO 1: Análise Léxica (--lexer)
        ["--lexer", fileName] -> do
            source <- readUtf8File fileName
            case runLexer source of
                Left err -> putStrLn $ "Erro Léxico: " ++ err
                Right tokens -> mapM_ print tokens

        -- MODO 2: Análise Sintática / AST (--parser)
        ["--parser", fileName] -> do
            source <- readUtf8File fileName
            case runLexer source of
                Left err -> putStrLn $ "Erro Léxico: " ++ err
                Right tokens -> do
                    let ast = parse tokens
                    -- Converte a AST para Tree e desenha na tela
                    putStrLn (drawTree (astToTree ast))

        -- MODO 3: Pretty Printer (--pretty)
        ["--pretty", fileName] -> do
            source <- readUtf8File fileName
            case runLexer source of
                Left err -> putStrLn $ "Erro Léxico: " ++ err
                Right tokens -> do
                    let ast = parse tokens
                    putStrLn (prettyPrint ast)

        -- MODO 4: Análise Semântica (--check)
        ["--check", fileName] -> do
            source <- readUtf8File fileName
            case runLexer source of
                Left err -> putStrLn $ "Erro Léxico: " ++ err
                Right tokens -> do
                    let ast = parse tokens
                    
                    case runTypeCheck ast of
                        Left err -> do
                            putStrLn "\n❌ ERRO SEMÂNTICO ENCONTRADO:"
                            putStrLn $ "   " ++ err
                            exitFailure
                        Right () -> do
                            putStrLn "\n✅ SUCESSO:"
                            putStrLn "   O programa está semanticamente correto."

        ["--codegen", fileName] -> runCodeGen fileName

        -- MODO 6: Execução / Interpretador (--run ou apenas o nome do arquivo)
        ["--run", fileName] -> runProgram fileName
        [fileName]          -> runProgram fileName -- Executa por padrão se passar só o arquivo

        -- Caso padrão: Ajuda
        _ -> printUsage

-- =============================================================================
-- FUNÇÃO AUXILIAR DE GERAÇÃO DE CÓDIGO
-- =============================================================================

runCodeGen :: FilePath -> IO ()
runCodeGen fileName = do
    source <- readUtf8File fileName
    case runLexer source of
        Left err -> putStrLn $ "Erro Léxico: " ++ err
        Right tokens -> do
            let ast = parse tokens
            
            -- 1. Verifica Tipos (Não geramos código se houver erro semântico)
            case runTypeCheck ast of
                Left err -> do
                    putStrLn "\n❌ ERRO SEMÂNTICO (Geração Abortada):"
                    putStrLn $ "   " ++ err
                    exitFailure
                Right () -> do
                    -- 2. Se tudo ok, gera o WAT
                    putStrLn ";; --- CODIGO WAT GERADO ---"
                    putStrLn (genProgram ast)
                    putStrLn ";; -------------------------"

-- =============================================================================
-- FUNÇÃO AUXILIAR DE EXECUÇÃO (INTERPRETADOR)
-- =============================================================================

runProgram :: FilePath -> IO ()
runProgram fileName = do
    source <- readUtf8File fileName
    case runLexer source of
        Left err -> putStrLn $ "Erro Léxico: " ++ err
        Right tokens -> do
            let ast = parse tokens
            
            -- 1. Primeiro verifica os tipos
            case runTypeCheck ast of
                Left err -> do
                    putStrLn "\n❌ ERRO SEMÂNTICO (Execução Abortada):"
                    putStrLn $ "   " ++ err
                    exitFailure
                Right () -> do
                    -- 2. Se não houver erros, executa!
                    interpret ast

-- =============================================================================
-- OUTRAS AUXILIARES
-- =============================================================================

readUtf8File :: FilePath -> IO String
readUtf8File file = do
    handle <- openFile file ReadMode
    hSetEncoding handle utf8
    hGetContents handle

printUsage :: IO ()
printUsage = do
    putStrLn "Uso do Compilador SL:"
    putStrLn "---------------------"
    putStrLn "  sl-compiler --lexer <arquivo.sl>    : Exibe a lista de tokens"
    putStrLn "  sl-compiler --parser <arquivo.sl>   : Exibe a Árvore Sintática (AST)"
    putStrLn "  sl-compiler --pretty <arquivo.sl>   : Exibe o código formatado"
    putStrLn "  sl-compiler --check <arquivo.sl>    : Verifica erros de tipos e escopo"
    putStrLn "  sl-compiler --codegen <arquivo.sl>  : Gera código WebAssembly (WAT)"
    putStrLn "  sl-compiler --run <arquivo.sl>      : Executa o programa (Interpretador)"
    putStrLn "  sl-compiler <arquivo.sl>            : Atalho para executar"
    putStrLn ""
    exitFailure