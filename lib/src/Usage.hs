module Java.Parser.Usagefinder where

import           Data.List
import           Data.List.Split
import           Language.Java.Parser
import           Language.Java.Syntax
import           Lib

data Import = StaticImp | NormalImp

data SearchBehaviour = SearchBehaviour {
    command       :: Command
    , needsImport ::Bool
    }

-- | isOriginal checks if the package declaration contains the type we are looking for, or if the TypeDeclartation contains the
-- type we are looking for. If either do, we are in the original file of the Type, unless this class extends the original class,
-- which it might.
isOrignal :: PackageDecl -> String -> TypeDecl -> String -> Bool
isOrignal pkgDecl pkg typeDecl cl = isOriginalPackage pkgDecl pkg && checkTypeDecl typeDecl cl
    where
        checkTypeDecl :: TypeDecl -> String -> Bool
        checkTypeDecl (ClassTypeDecl (ClassDecl _ (Ident s) _ _ refTypes _)) cl
            | s == cl = True
            | otherwise = checkRefType refTypes cl
        checkTypeDecl (ClassTypeDecl (EnumDecl _ (Ident s) refTypes _)) cl
            | s == cl = True
            | otherwise = checkRefType refTypes cl
        checkTypeDecl (InterfaceTypeDecl (InterfaceDecl _ _ (Ident s) _ refTypes _)) cl
            | s == cl = True
            | otherwise = checkRefType refTypes cl
        checkRefType :: [RefType] -> String -> Bool
        checkRefType refTypes cl = any (`compareType` cl) refTypes
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


