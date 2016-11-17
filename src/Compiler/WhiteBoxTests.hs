module Compiler.WhiteBoxTests where

import Compiler.Impl

import Test.Tasty
import Test.Tasty.Golden

import System.FilePath ( (</>) ) 

golden' :: Read a => String -> (a -> CmplM ()) -> TestTree
golden' ident f =
  goldenVsFile name fref fout $ do
    a <- readFile fin
    case compile f $ read a of
      Left e -> writeFile fout (show e)
      Right s -> writeFile fout s
  where
    prefix = "examples"
    basename = ident
    name = basename ++ ".fun.ast"
    fin = prefix </> name
    fref = prefix </> basename ++ ".ref.c"
    fout = prefix </> basename ++ ".out.c"

goldenTests :: TestTree
goldenTests = testGroup "Golden Tests"
  [ golden' "empty" cmplScript
  , golden' "true" cmplScript
  , golden' "args" cmplScript
  , golden' "2clones" cmplScript
  ]

main :: IO ()
main = defaultMain $ testGroup "Tests" [goldenTests]
