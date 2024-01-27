import           System.FilePath   (replaceExtension, takeBaseName)
import           System.IO
import           Test.Tasty        (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden (findByExtension, goldenVsFileDiff)
import           Text.Parsec.Prim  (parse)

import           Data.Char         (chr, ord)
import           Data.Map          (elems)
import qualified SlangLib

main :: IO ()
main = defaultMain =<< goldenTests

execute srcFile inFile outFile = do
  libContents <- readFile "src/Prelude.asm"
  srcContents <- readFile srcFile
  let result = parse SlangLib.program srcFile srcContents
  case result of
    Left err -> writeFile outFile $ show err
    Right program -> do
      let instructions = SlangLib.tranlsate program
      let instructions' = instructions
      let instructionsOut = concatMap (\x -> show x ++ "\n") instructions' ++ "\n" ++ libContents
      writeFile outFile instructionsOut

goldenTests :: IO TestTree
goldenTests = do
  slangFiles <- findByExtension [".sl"] "golden"
  return $ testGroup "Slang golden tests"
    [ goldenVsFileDiff
        (takeBaseName slangFile) -- test name
        (\ref new -> ["diff", "-u", ref, new]) -- diff
        cmpFile -- golden file path
        outFile
        (execute slangFile inFile outFile) -- action whose result is tested
    | slangFile <- slangFiles
    , let cmpFile = replaceExtension slangFile ".asm"
    , let inFile = replaceExtension slangFile ".in"
    , let outFile = replaceExtension slangFile ".out"
    ]
