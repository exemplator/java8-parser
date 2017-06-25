module Lib where

import           Data.List
import           Data.List.Split
import           Language.Java.Parser
import           Language.Java.Syntax

data Import = StaticImp | NormalImp

-- TODO: Add return true if current class implements or extends the original class 
isOrignal :: PackageDecl -> String -> TypeDecl -> String -> Bool
isOrignal pkgDecl pkg typeDecl cl = isOriginalPackage pkgDecl pkg && checkTypeDecl typeDecl cl
    where
        checkTypeDecl :: TypeDecl -> String -> Bool
        checkTypeDecl (ClassTypeDecl classDecl) cl = False
        checkTypeDecl (InterfaceTypeDecl (InterfaceDecl _ _ (Ident s) _ refTypes _)) cl
            | s == cl = True
            | otherwise = any (\refTyp -> compareType refTyp cl) refTypes
        compareType :: RefType -> String -> Bool -- TODO: Implement this method correctly
        compareType _ _ = True

isOriginalPackage :: PackageDecl -> String -> Bool
isOriginalPackage (PackageDecl name) package = splitPackage package == nameToString name

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


