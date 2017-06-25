module Java.Parser.Lib
    ( parse
    , Command
    , Position
    , Selection
    ) where

import           Language.Java.Parser

data Input = String Command

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


parse :: String -> String
parse input = toString (result input)
    where
        toString (Right _) = "parsed successfully"
        toString _ = "error while parsing"
        result input = parser compilationUnit input



someFunc :: IO ()
someFunc = putStrLn "someFunc"
