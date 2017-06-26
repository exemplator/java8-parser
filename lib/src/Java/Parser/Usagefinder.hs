module Java.Parser.UsageFinder where

import           Data.List
import           Data.List.Split
import           Java.Parser.Lib
import           Java.Parser.Typechecker
import           Language.Java.Parser
import           Language.Java.Syntax

-- |
-- the high level dfs-ast algorithm
--

nameToString :: Name -> [String]
nameToString (Name idents) = map (\(Ident x) -> x) idents

splitPackage :: String -> [String]
splitPackage = splitOn "."
