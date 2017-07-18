module Java.Parser.Old.Internals where

import           Control.Applicative
import           Java.Parser.Lib
import           Language.Java.Syntax

-- |
-- Common data and function definitions
--

data SearchBehaviour = SearchBehaviour {
    command       :: Command
    , needsImport ::Bool
    }

-- | combines the Alternatives if both present, if not returns the one present
combine :: Alternative f => f a -> f a -> (a -> a -> a) -> f a
combine x y f = liftA2 f x y <|> x <|> y

identS :: Ident -> String
identS (Ident x) = x
