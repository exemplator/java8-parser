module Lib where

import           Data.List
import           Data.List.Split
import           Language.Java.Parser
import           Language.Java.Syntax

data Import = StaticImp | NormalImp


isOriginal :: PackageDecl -> String -> Bool
isOriginal (PackageDecl name) package = splitPackage package == nameToString name

isImported :: ImportDecl -> String -> Maybe Import
isImported (ImportDecl static importName generic) package
    | checkImported generic = Just correctImport
    | otherwise = Nothing
    where
        checkImported :: Bool -> Bool
        checkImported True = iName `isPrefixOf` splittedPackages
        checkImported False = iName == splittedPackages
        iName = nameToString importName
        splittedPackages = splitPackage package
        correctImport = if static then StaticImp else NormalImp

nameToString :: Name -> [String]
nameToString (Name idents) = map (\(Ident x) -> x) idents

splitPackage :: String -> [String]
splitPackage = splitOn "."
