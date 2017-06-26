module Java.Parser.DeclarationChecker where

import           Data.List
import           Data.List.Split
import           Java.Parser.Lib
import           Java.Parser.Typechecker
import           Language.Java.Parser
import           Language.Java.Syntax

-- |
-- provides functions to check package, imports and field and variable declarations.
--

data Import = StaticImp | NormalImp

-- | isOriginal checks if the package declaration contains the type we are looking for, or if the TypeDeclartation contains the
-- type we are looking for. If either do, we are in the original file of the Type, unless this class extends the original class,
-- which it might.
isOrignal :: PackageDecl -> TypeDecl -> SearchBehaviour -> Bool
isOrignal pkgDecl typeDcl behavior = fromMaybe True combine correctPackage correctType (&&)
    where
        checkTypeDecl :: TypeDecl -> String -> Bool
        checkTypeDecl (ClassTypeDecl (ClassDecl _ ident _ _ ref _)) = compareToString ident ref
        checkTypeDecl (ClassTypeDecl (EnumDecl _ ident ref _)) = compareToString ident ref
        checkTypeDecl (InterfaceTypeDecl (InterfaceDecl _ _ ident _ ref _)) = compareToString ident ref

        compareToString (Ident s) refTypes cl = (s == cl) || checkRefType refTypes cl
        checkRefType :: [RefType] -> Bool
        checkRefType = any (`checkType` behavior)
        com = command behavior
        correctPackage = fmap (isOriginalPackage pkgDecl) (packageName com)
        correctType = fmap (checkTypeDecl typeDcl) (className com)

isOriginalPackage :: PackageDecl -> String -> Bool
isOriginalPackage (PackageDecl pName) package = nameToString pName == splitPackage package

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
