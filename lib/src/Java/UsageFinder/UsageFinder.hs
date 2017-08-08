module Java.UsageFinder.UsageFinder where

import           Java.UsageFinder.Lib
import           Language.Java.Parser
import           Language.Java.Syntax
import           Language.Java.Position
import           Text.Parsec            hiding (Empty)
import           Java.UsageFinder.DFSTraversal

-- |
-- the high level dfs-ast algorithm
--

-- findOccurances :: Command -> String -> Either String [Segment]
-- findOccurances command code =  traverseAST command <$> parseCompilationUnit code
