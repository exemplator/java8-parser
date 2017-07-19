module Java.UsageFinder.Lib
    ( Command
    , packageName
    , className
    , methodName
    ) where

import           Language.Java.Position

-- |
-- provides a simple interface to use the Java parser
--

data Input = Input String Command

data Command = Command {
    packageName  :: Maybe String
    , className  :: Maybe String
    , methodName :: Maybe String
    } deriving (Show)

getSelections :: Input -> Either String [Segment]
getSelections input = undefined
