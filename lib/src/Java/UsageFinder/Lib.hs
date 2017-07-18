module Java.UsageFinder.Lib
    ( Command
    , packageName
    , className
    , methodName
    , Position
    , line
    , column
    , Selection
    , start
    , end
    ) where

-- |
-- provides a simple interface to use the Java parser
--

data Input = Input String Command

data Command = Command {
    packageName  :: Maybe String
    , className  :: Maybe String
    , methodName :: Maybe String
    } deriving (Show)

data Position = Position {
    line     :: Integer
    , column :: Integer
    } deriving (Show)

data Selection = Selection {
    start :: Position,
    end   :: Position
    } deriving (Show)

getSelections :: Input -> Either String [Selection]
getSelections input = undefined
