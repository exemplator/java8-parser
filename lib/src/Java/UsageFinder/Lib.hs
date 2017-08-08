module Java.UsageFinder.Lib
    ( Command
    , packageNameCommand
    , classNameCommand
    , methodNameCommand
    ) where

import           Language.Java.Position
import           Language.Java.Syntax

-- |
-- provides a simple interface to use the Java parser
--

data Input = Input String Command

data Command = Command {
    packageNameCommand  :: Maybe String
    , classNameCommand  :: Maybe String
    , methodNameCommand :: Maybe String
    } deriving (Show)

getSelections :: Input -> Either String [Segment]
getSelections input = undefined

-- rValueFromFieldDecl :: MemberDecl l -> [String]
-- rValueFromFieldDecl (FieldDecl _ _ _ varDeclList) = map rValueFromVarDecl varDeclList
--     where 
--         rValueFromVarDecl :: VarDecl l -> String
--         rValueFromVarDecl (VarDecl _ (VarId _ (Ident val)) _) = val
