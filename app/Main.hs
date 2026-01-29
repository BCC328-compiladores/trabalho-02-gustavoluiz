module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, openFile, stdout, utf8)

-- Importações do Compilador
import Lexer (runLexer)
import Parser (parse)
import Pretty (prettyPrint)

-- NOVAS IMPORTAÇÕES (Necessárias para o --parser funcionar com drawTree)
import Data.Tree (drawTree)
import TreeUtils (astToTree)

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

        -- Caso padrão: Ajuda
        _ -> printUsage

-- =============================================================================
-- FUNÇÕES AUXILIARES
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
    putStrLn "  sl-compiler --lexer <arquivo.sl>   : Exibe a lista de tokens"
    putStrLn "  sl-compiler --parser <arquivo.sl>  : Exibe a Árvore Sintática (AST)"
    putStrLn "  sl-compiler --pretty <arquivo.sl>  : Exibe o código formatado (Pretty Print)"
    putStrLn ""
    exitFailure