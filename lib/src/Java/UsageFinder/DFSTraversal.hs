module Java.UsageFinder.DFSTraversal where

import           Java.UsageFinder.Lib
import           Language.Java.Syntax
import           Language.Java.Position
import           Data.Maybe (mapMaybe)

type TargetType = RelaxedType
type TargetMethod = String
type DeclarationCheck l = ([ImportDecl l], Bool)

data Target = Target 
    { targetType :: TargetType
    , targetMethod :: Maybe TargetMethod
    }

data Import = StaticImp | NormalImp

traverseAST :: Target -> CompilationUnit Segment -> [Selection]
traverseAST target cu = isImported target cu ++ isOrignal target cu

isImported :: Target -> CompilationUnit Segment -> [Selection]
isImported target (CompilationUnit l pckDecl importDecls typeDecl) = mapMaybe (scanImported (targetType target)) importDecls
    where
        scanImported :: TargetType -> ImportDecl l -> Maybe (ImportDecl l)
        scanImported targetType (ImportDecl l True pkg) = if targetType == RelaxedType (getType pkg) then Just (ImportDecl l True pkg) else Nothing
        scanImported targetType (ImportDecl l False pkg) = if targetType == RelaxedType (getType pkg) then Just (ImportDecl l False pkg) else Nothing

isOrignal ::  Target -> CompilationUnit Segment -> [Selection]
isOrignal = undefined

isOriginalPackage :: TargetType -> PackageDecl l -> TypeDecl l -> [ImportDecl l]
isOriginalPackage = undefined


