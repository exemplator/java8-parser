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

-- | Data Structure to track any encountered type 
--  Type: Type of AST node (ex. import statement, extends, implements, but also variable types) 
--  Maybe Ident: If the AST Node has a name as well as a type, then the name of the node (ex. variable name)
--- Example: Integer x       -> Type: Integer   , Name: x 
--- Class extends SuperClass -> Type: SuperClass, Name: Nothing
type PossibleTypeMatch l = (Type, Maybe Ident, Result l)

-- | Data Structure to track any encountered method 
type PossibleMethodMatch l = (MethodType, MethodName, Result l)

-- | Take two search contexts and merge results
mergeUpstream :: SearchContext l -> SearchContext l -> SearchContext l
mergeUpstream = undefined

getResults :: SearchContext l -> [Result l]
getResults = undefined

handleType :: SearchContext l -> PossibleTypeMatch l -> SearchContext l
handleType ctx (t, ident, pRes) = 
    if (targetType . target) ctx == RelaxedType t
    then addTypeToState (t, ident, pRes) (maybe False shadow ident) ctx

handleMethod :: SearchContext l -> PossibleMethodMatch l -> SearchContext l
handleMethod = undefined

addTypeToState :: PossibleTypeMatch l -> Bool -> SearchContext l -> SearchContext l
addTypeToState (t, ident, pRes) ctx = (state ctx

shadow :: Ident -> SearchContext l -> Bool
shadow = undefined