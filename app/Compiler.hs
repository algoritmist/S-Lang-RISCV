module Main(main) where
import           SlangLib
import           System.Environment.Blank (getArgs)
import           System.FilePath          (replaceExtension, takeBaseName)
import           System.IO
import           Text.Parsec.Prim         (parse)
main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
        then do
            putStrLn "usage: slang-compiler <path_to_source.sl> <path_to_dest.asm>"
    else do
        let file = head args
        let outBin = last args
        fileContents <- readFile file
        libContents <- readFile "stdlib/Prelude.asm"
        let result = parse program file fileContents
        case result of
            Left err -> print err
            Right program -> do
                let instructions = tranlsate program
                writeFile outBin $ (concatMap (\x -> show x ++ "\n") instructions) ++ libContents