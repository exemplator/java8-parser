module Java.UsageFinder.DFSTraversal where

import           Java.UsageFinder.Lib
import           Language.Java.Syntax
import           Language.Java.Position

type TargetType = RelaxedType
type TargetMethod = String
type DeclarationCheck l = ([ImportDecl l], Bool)

data Target = Target 
    { targetType :: TargetType
    , targetMethod :: Maybe TargetMethod
    }

data TypeSource = TypeSource
    { inPackageScope :: Bool
    , inClassScope :: Bool 
    }

data Result l = Stuff (ImportDecl l) | RImportDecl (ImportDecl l)
    deriving (Show, Eq)

traverseAST :: Target -> CompilationUnit l -> [Result l]
traverseAST = isImported 

isImported :: Target -> CompilationUnit l -> [Result l]
isImported target (CompilationUnit l pkgDcl importDecls tDcl) = importDecls >>= scanImported (targetType target)
    where
        scanImported :: TargetType -> ImportDecl l -> [Result l]
        scanImported targetType (ImportDecl l True pkg) = if targetType == RelaxedType (getType pkg) 
            then isOrignal target TypeSource {inPackageScope = False, inClassScope = True} [RImportDecl (ImportDecl l True pkg)] (CompilationUnit l pkgDcl importDecls tDcl)   -- A static import is matched
            else isOrignal target TypeSource {inPackageScope = False, inClassScope = False} [] (CompilationUnit l pkgDcl importDecls tDcl)                                                                                                       -- No static is no static import
        scanImported targetType (ImportDecl l False pkg) = if targetType == RelaxedType (getType pkg)   
            then isOrignalWithImport target [RImportDecl (ImportDecl l True pkg)] (CompilationUnit l pkgDcl importDecls tDcl)        -- An import is matched
            else isOrignalWithImport target [] (CompilationUnit l pkgDcl importDecls tDcl)                                           -- No import is matched

isOrignal ::  Target -> TypeSource -> CompilationUnit l -> [Result l]
isOrignal = undefined

isOriginalPackage :: TargetType -> PackageDecl l -> TypeDecl l -> [ImportDecl l]
isOriginalPackage = undefined

parse :: Target -> [Result l] -> CompilationUnit l -> [Result l]
parseWithImported = undefined

parse :: Target -> [Result l] -> CompilationUnit l -> [Result l]
parseWithoutImported = undefined

