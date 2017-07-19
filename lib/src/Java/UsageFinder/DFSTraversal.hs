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

        isOriginalPackage = fromMaybe False (comp target <$> pkgDcl)

        parseResults = tDcl >>= traverseTypeDecl isOriginalPackage tSource

traverseTypeDecl :: Bool -> SearchBehaviour -> TypeDecl l-> [Result l]
traverseTypeDecl isOriginalPackage (target, typeSource) typeDecl = next
    where
        isOriginal = target == (RelaxedType . getType) typeDecl && isOriginalPackage
        extendsImplements = not $ null (comp target <$> delete (getType typeDecl) collectTypes)
                            && inPackageScope typeSource
        updatedSeachBehaviour = updateClassScope (isOriginal || extendsImplements) typeSource
        next = traverseBody (target, updatedSeachBehaviour) typeDecl

traverseBody :: SearchBehaviour -> TypeDecl l-> [Result l]
traverseBody = undefined

comp :: (HasType a) => RelaxedType -> HasType a -> Bool
comp rel hasT = rel == (RelaxedType . getType) hasT
