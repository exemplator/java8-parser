module Java.UsageFinder.DFSTraversal where

import           Data.List                (delete)
import           Data.Maybe               (fromMaybe)
import           Java.UsageFinder.Lib
import           Language.Java.Position
import           Language.Java.Syntax
import           Language.Java.Helper

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

type SearchBehavior = (TargetType, TypeSource)

defaultTypeSource = TypeSource{inPackageScope=False, inClassScope=False}

updatePkgScope bool tSource = TypeSource{inPackageScope=bool, inClassScope=inClassScope tSource}
updateClassScope bool tSource = TypeSource{inPackageScope=inPackageScope tSource, inClassScope=bool}


data Result l = Stuff (ImportDecl l) | RImportDecl (ImportDecl l)
    deriving (Show, Eq)

instance Eq l => Ord (Result l) where
    compare a b = compare (toInt a) (toInt b)
        where
            toInt Stuff{} = 1
            toInt RImportDecl{} = 2

traverseAST :: TargetType -> CompilationUnit l -> [Result l]
traverseAST target (CompilationUnit l pkgDcl importDecls tDcl) = parseResults ++ results
    where
        (tSource, results) = foldr checkImport (defaultTypeSource, []) importDecls
        checkImport importDecl (typeSource, results) = if comp target importDecl
            then (updateSource typeSource importDecl, RImportDecl importDecl:results)
            else (typeSource, results)

        updateSource tSource (ImportDecl _ True _) = updateClassScope True tSource
        updateSource tSource (ImportDecl _ False _) = updatePkgScope True tSource

        isOriginalPackage = fromMaybe False (comp target <$> pkgDcl)

        parseResults = tDcl >>= traverseTypeDecl isOriginalPackage (target, tSource)

traverseTypeDecl :: Bool -> SearchBehavior -> TypeDecl l-> [Result l]
traverseTypeDecl isOriginalPackage (target, typeSource) typeDecl = next
    where
        isOriginal = comp target typeDecl && isOriginalPackage
        everyExtendImplement = delete (getType typeDecl) (collectTypes typeDecl)
        doesExtendImplement = not $ null (map (comp target) everyExtendImplement)
                            && inPackageScope typeSource
        updatedSearchBehavior = updateClassScope (isOriginal || doesExtendImplement) typeSource
        next = traverseBody (target, updatedSearchBehavior) typeDecl

traverseBody :: SearchBehavior -> TypeDecl l -> [Result l]
traverseBody sb td = getBody td >>= traverseDecl sb

traverseDecl :: SearchBehavior -> Decl l -> [Result l]
traverseDecl sb (MemberDecl _ decl) = traverseMemberDecl sb decl
traverseDecl sb (InitDecl _ static block) = undefined

traverseMemberDecl :: SearchBehavior -> MemberDecl l -> [Result l]
traverseMemberDecl sb (FieldDecl _ _ t varDecls) = undefined
traverseMemberDecl sb (MethodDecl _ _ _ t name params exT _ body) = undefined
traverseMemberDecl sb (ConstructorDecl _ _ t _ _ _ _) = undefined
traverseMemberDecl sb (MemberClassDecl _ _ ) = undefined
traverseMemberDecl sb (MemberInterfaceDecl _ _ ) = undefined

comp :: (HasType a) => RelaxedType -> a -> Bool
comp rel = (rel ==) . RelaxedType . getType
