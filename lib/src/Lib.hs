module Lib
    ( parse
    ) where

import Language.Java.Parser

parse :: String -> String
parse input = toString (result input)
    where 
        toString (Right _) = "parsed successfully"
        toString _ = "error while parsing"
        result input = parser compilationUnit input

someFunc :: IO ()
someFunc = putStrLn "someFunc"