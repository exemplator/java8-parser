module Java.UsageFinder.DFSTraversal where

import           Java.UsageFinder.Lib
import           Language.Java.Position
import           Language.Java.Syntax

type TargetType = RelaxedType
type TargetMethod = String
type DeclarationCheck l = ([ImportDecl l], Bool)

data Target = Target
    { targetType   :: TargetType
    , targetMethod :: Maybe TargetMethod
    }

data TypeSource = TypeSource
    { inPackageScope :: Bool
    , inClassScope   :: Bool
    }

type SearchBehaviour = (Target, TypeSource)

defaultTypeSource = TypeSource{inPackageScope=False, inClassScope=False}

updatePkgScope bool tSource = TypeSource{inPackageScope=bool, inClassScope=inClassScope typeSource}
updateClassScope bool tSource = TypeSource{inPackageScope=inPackageScope tSource, inClassScope=bool}


data Result l = Stuff (ImportDecl l) | RImportDecl (ImportDecl l)
    deriving (Show, Eq)

traverseAST :: Target -> CompilationUnit l -> [Result l]
traverseAST = isImported


traverseDeclarations :: TargetType -> CompilationUnit l -> [Result l]
traverseDeclarations target (CompilationUnit l pkgDcl importDecls tDcl) = parseResults ++ results
    where
        (tSource, results) = foldr checkImport (defaultTypeSource, []) importDecls
        checkImport (typeSource, results) importDecl = if target == RelaxedType $ getType importDecl
            then (updateSource typeSource importDecl, RImportDecl importDecl:results)
            else (typeSource, results)

        updateSource tSource (ImportDecl _ True _) = updateClassScope True tSource
        updateSource tSource (ImportDecl _ False _) = updatePkgScope True tSource

        isOriginalPackage = fromMaybe False ((\pkg -> target == (RelaxedType . getType) pkg) <$> pkgDcl)

        parseResults = tDcl >>= traverseTypeDecl isOriginalPackage tSource

traverseTypeDecl :: Bool -> SearchBehaviour -> TypeDecl l-> [Result l]
traverseTypeDecl isOriginalPackage (target, typeSource) typeDecl = next
    where
        isOriginal = target == (RelaxedType . getType) typeDecl
        updatedSeachBehaviour = updateClassScope True typeSource
        next = traverseBody (target, updatedSeachBehaviour) typeDecl

traverseBody :: SearchBehaviour -> TypeDecl l-> [Result l]
traverseBody = undefined





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

