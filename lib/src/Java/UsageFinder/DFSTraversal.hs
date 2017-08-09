module Java.UsageFinder.DFSTraversal where

import           Data.List                (delete)
import           Data.Maybe               (fromMaybe, isNothing, isJust, fromJust)
import           Java.UsageFinder.Lib
import           Language.Java.Position
import           Language.Java.Syntax
import           Language.Java.Helper
import           Java.UsageFinder.TraversalContext

type DeclarationCheck l = ([ImportDecl l], Bool)

type SearchBehavior = (Target, TypeSource)

defaultTypeSource = TypeSource{inPackageScope=False, inClassScope=False}

updatePkgScope bool tSource = TypeSource{inPackageScope=bool, inClassScope=inClassScope tSource}
updateClassScope bool tSource = TypeSource{inPackageScope=inPackageScope tSource, inClassScope=bool}

traverseAST :: Target -> CompilationUnit l -> [Result l]
traverseAST (Target tType tMethod) (CompilationUnit l pkgDcl importDecls tDcl) = parseResults ++ results
    where
        (tSource, results) = foldr checkImport (defaultTypeSource, []) importDecls
        checkImport importDecl (typeSource, results) = if comp tType importDecl
            then (updateSource typeSource importDecl, RImportDecl importDecl:results)
            else (typeSource, results)

        updateSource tSource (ImportDecl _ True _) = updateClassScope True tSource
        updateSource tSource (ImportDecl _ False _) = updatePkgScope True tSource

        isOriginalPackage = fromMaybe False (comp tType <$> pkgDcl)

        parseResults = tDcl >>= traverseTypeDecl isOriginalPackage (Target tType tMethod, tSource)

traverseTypeDecl :: Bool -> SearchBehavior -> TypeDecl l-> [Result l]
traverseTypeDecl isOriginalPackage (Target tType tMethod, typeSource) typeDecl = next
    where
        isOriginal = comp tType typeDecl && isOriginalPackage
        everyExtendImplement = delete (getType typeDecl) (collectTypes typeDecl)
        doesExtendImplement = not $ null (map (comp tType) everyExtendImplement) && inPackageScope typeSource
        updatedSearchBehavior = updateClassScope (isOriginal || doesExtendImplement) typeSource
        next = traverseBody (Target tType tMethod, updatedSearchBehavior) typeDecl

traverseBody :: SearchBehavior -> TypeDecl l -> [Result l]
traverseBody sb td = getBody td >>= traverseDecl sb

traverseDecl :: SearchBehavior -> Decl l -> [Result l]
traverseDecl sb (MemberDecl _ decl) = traverseMemberDecl sb decl
traverseDecl sb (InitDecl _ static block) = undefined

traverseMemberDecl :: SearchBehavior -> MemberDecl l -> [Result l]
traverseMemberDecl (Target tType tMethod, _) (FieldDecl info mdm t varDecls) =
    [RMemberDecl (FieldDecl info mdm t varDecls) | RelaxedType t == tType && isNothing tMethod]
traverseMemberDecl (Target tType tMethod, TypeSource inPkgScp inClSkp) (MethodDecl info mdm mtp rt (Ident name) params e dia body) =
    if inClSkp && isJust tMethod && name == fromJust tMethod
    then [RMemberDecl (MethodDecl info mdm mtp rt (Ident name) params e dia body)] 
    else traverseMethodBody (Target tType tMethod, TypeSource inPkgScp inClSkp) body
traverseMemberDecl (Target tType tMethod, TypeSource inPkgScp inClSkp) (ConstructorDecl info mod tp (Ident name) fp e body) =
    if inClSkp && isJust tMethod && name == fromJust tMethod
    then [RMemberDecl (ConstructorDecl info mod tp (Ident name) fp e body)] 
    else traverseConstructorBody (Target tType tMethod, TypeSource inPkgScp inClSkp) body
traverseMemberDecl sb (MemberClassDecl info memClDecl) = getBody memClDecl >>= traverseDecl sb
traverseMemberDecl sb (MemberInterfaceDecl info memIterDecl ) = getBody memIterDecl >>= traverseDecl sb

traverseConstructorBody :: SearchBehavior -> ConstructorBody l -> [Result l]
traverseConstructorBody = undefined

traverseMethodBody :: SearchBehavior -> MethodBody l -> [Result l]
traverseMethodBody = undefined

comp :: (HasType a) => RelaxedType -> a -> Bool
comp rel = (rel ==) . RelaxedType . getType
