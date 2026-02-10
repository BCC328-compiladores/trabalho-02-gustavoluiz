module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Exit (exitFailure, exitSuccess)
import Data.List (isPrefixOf, sort)
import Control.Exception (catch, evaluate, SomeException)

-- Imports de leitura de arquivo com encoding
import System.IO (openFile, IOMode(ReadMode), hSetEncoding, utf8, hGetContents)

import Lexer (runLexer)
import Parser (parse)
import TypeChecker (runTypeCheck)

testDir :: FilePath
testDir = "src/tests"

-- LISTA DE ARQUIVOS PARA IGNORAR (Features incompletas)
ignoredTests :: [String]
ignoredTests = 
    [ "test2_structs.sl"  -- Ignora structs por enquanto
    , "test3_arrays.sl"
    , "test4_generics.sl"
    , "test_broken_comment.sl"
    , "test_erro_condicao.sl" --caso erro intecional
    , "test_erro_escopo.sl"
    , "test_erro_funcao.sl"
    , "test_erro_struct_array.sl"
    , "test_erro_tipos.sl"
    ]

main :: IO ()
main = do
    putStrLn "=== Iniciando Suite de Testes do SL Compiler ==="
    
    allFiles <- listDirectory testDir
    let slFiles = sort $ filter (\f -> takeExtension f == ".sl") allFiles

    -- Filtra: Não começa com "erro_" E não está na lista de ignorados
    let successTests = filter (\f -> not (isPrefixOf "erro_" f) && f `notElem` ignoredTests) slFiles

    putStrLn $ "Encontrados " ++ show (length slFiles) ++ " arquivos .sl totais."
    putStrLn $ "Executando " ++ show (length successTests) ++ " testes (ignorando structs/erros)."

    putStrLn "\n--- Rodando Testes de Sucesso Esperado ---"
    mapM_ runSuccessTest successTests

    putStrLn "\n=== Todos os testes selecionados passaram! ==="
    exitSuccess

runSuccessTest :: FilePath -> IO ()
runSuccessTest fileName = do
    let fullPath = testDir </> fileName
    putStr $ "Testando " ++ fileName ++ "... "
    
    handle <- openFile fullPath ReadMode
    hSetEncoding handle utf8
    content <- hGetContents handle
    
    case runLexer content of
        Left err -> failTest $ "Erro Lexico: " ++ err
        Right tokens -> do
            catch (do
                let ast = parse tokens
                _ <- evaluate ast 
                
                case runTypeCheck ast of
                    Left err -> failTest $ "Erro Semantico: " ++ err
                    Right _ -> putStrLn "OK"
                
                ) handler
  where
    handler :: SomeException -> IO ()
    handler ex = failTest $ "Erro Sintatico (Parser Crash): " ++ show ex

failTest :: String -> IO ()
failTest msg = do
    putStrLn "FALHOU"
    putStrLn $ "  -> " ++ msg
    exitFailure