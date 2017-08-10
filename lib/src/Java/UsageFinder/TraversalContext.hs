{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module Java.UsageFinder.TraversalContext where

import           Data.Function        (on)
import           Language.Java.Syntax

type TargetType = RelaxedType
type TargetMethod = String
type ResultType = RelaxedType
type ResultValue = String

data Target = Target
    { targetType   :: TargetType
    , targetMethod :: Maybe TargetMethod
    }

data TypeSource = TypeSource
    { inPackageScope :: Bool    -- package is in scope, but class is not (i.e. NOT static imports)
    , inClassScope   :: Bool    -- class is in scope (i.e. static imports or defined in this class)
    }

-- | All possible result types that are ordered through their importance. Imports are less important then everything else.
--   Rxxxxx = Result xxxxx
data Result l =
    RMemberDecl (MemberDecl l)
    | RTypeDecl (TypeDecl l)
    | RExtends (Extends l)
    | RImplements (Implements l)
    | RImportDecl (ImportDecl l)
    deriving (Show, Eq)

instance Eq l => Ord (Result l) where
    compare a b = compare (toInt a) (toInt b)
        where
            toInt RMemberDecl{} = 1
            toInt RTypeDecl{} = 2
            toInt RImportDecl{} = 3

-- | Represents the state of stored data
-- vars: The boolean says whether the variable has the same type as the one we are looking for, and the Ident is the name of the variable
data SearchState = SearchState
    { thisPointer :: Ident
    , vars        :: [(Bool, Ident)]
    }

data SearchContext l = SearchContext
    { target      :: Target
    , typeSource  :: TypeSource
    , state       :: [SearchState]
    , imports     :: [ImportDecl l]
    , thisPointer :: Ident
    , result      :: [Result l]
    }

data MethodType = MVar Ident | MThis (Maybe Ident) | MType Type
type MethodName = Ident

-- | Data Structure to track any encountered type 
--  Type: Type of AST node (ex. import statement, extends, implements, but also variable types) 
--  Maybe Ident: If the AST Node has a name as well as a type, then the name of the node (ex. variable name)
--- Example: Integer x       -> Type: Integer   , Name: x 
--- Class extends SuperClass -> Type: SuperClass, Name: Nothing
type PossibleTypeMatch l = (Type, Maybe Ident, Result l)

-- | Data Structure to track any encountered method 
type PossibleMethodMatch l = (MethodType, MethodName, Result l)

type IsSameType = Bool
type IsNewScope = Bool

setInPackageScope :: Bool -> SearchContext l -> SearchContext l
setInPackageScope = undefined

setInClassScope :: Bool -> SearchContext l -> SearchContext l
setInClassScope = undefined


-- | Take two search contexts and merge results
mergeUpstream :: SearchContext l -> SearchContext l -> SearchContext l
mergeUpstream = undefined

getResults :: SearchContext l -> [Result l]
getResults = undefined

setPckDecl :: SearchContext l -> PackageDecl l -> SearchContext l
setPckDecl = undefined

addImportStatement :: SearchContext l -> ImportDecl l -> SearchContext l
addImportStatement = undefined

-- | When we encounter a new class (inner class etc.) we create a new this pointer and then create a new search state for that scope 
setThisPointer :: Ident -> SearchContext l -> SearchContext l
setThisPointer ident ctx = SearchContext {thisPointer = ident, state = SearchState ident [] : state ctx}

-- TODO handle correct package resolution!
-- TODO handle Imports & TypeDecls (ClassScope, PackageScope)
-- | handle case where we are handling classDecls and ImportStatements!
-- Add variable if:
--   1. Types are the same
--   2. Variable shadows another variable (irrelevant of types)
handleType :: SearchContext l -> PossibleTypeMatch l -> SearchContext l
handleType ctx (t, ident, pRes) =
    if isSameType || isShadowed
    then addTypeToState (t, ident, pRes) isSameType isShadowed ctx
    else undefined
    where
        isSameType = typeEquals ctx (t, ident, pRes)
        isShadowed = maybe False (shadow ident ctx) ident

handleMethod :: SearchContext l -> PossibleMethodMatch l -> SearchContext l
handleMethod = undefined

-- | Checks if type is equal to target type
typeEquals :: SearchContext l -> PossibleTypeMatch l -> Bool
typeEquals ctx (t, ident, pRes) = (targetType . target) ctx == RelaxedType t

-- TODO: handle thisPointer 
-- | Add type to the search context
addTypeToState :: PossibleTypeMatch l -> IsSameType -> IsNewScope -> SearchContext l -> SearchContext l
addTypeToState (t, ident, pRes) ist ins ctx = if ins then (ist, ident) : (vars . head . state) ctx
    else SearchState (thisPointer ctx) [(iSt, ident)] : state ctx

-- | Checks if ident is shadowing some variable we already stored 
shadow :: Ident -> SearchContext l -> Bool
shadow ident ctx = foldr (\s b1 -> b1 || elem ident (map snd $ vars s)) False $ state ctx
