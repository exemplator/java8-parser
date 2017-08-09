module Java.UsageFinder.TraversalContext where

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
    | RImportDecl (ImportDecl l)
    deriving (Show, Eq)

instance Eq l => Ord (Result l) where
    compare a b = compare (toInt a) (toInt b)
        where
            toInt RMemberDecl{} = 1
            toInt RImportDecl{} = 2

data SearchState = SearchState
    { thisPointer :: Ident
    , vars        :: [Ident]
    }

data SearchContext l = SearchContext
    { target     :: Target
    , typeSource :: TypeSource
    , state      :: [SearchState]
    , result     :: [Result l]
    }

data MethodType = MVar Ident | MThis (Maybe Ident) | MType Type
type MethodName = Ident
type PossibleTypeMatch l = (Type, Result l)
type PossibleMethodMatch l = (MethodType, MethodName, Result l)

-- | Take two search contexts and merge results
mergeUpstream :: SearchContext l -> SearchContext l -> SearchContext l
mergeUpstream = undefined

getResults :: SearchContext l -> [Result l]
getResults = undefined

handleType :: SearchContext l -> PossibleTypeMatch l -> SearchContext l
handleType = undefined

handleMethod :: SearchContext l -> PossibleMethodMatch l -> SearchContext l
handleMethod = undefined
