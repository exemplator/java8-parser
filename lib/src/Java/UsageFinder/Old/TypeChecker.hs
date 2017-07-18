module Java.Parser.Old.TypeChecker where

import           Data.Char
import           Data.Maybe
import           Java.Parser.Internals
import           Java.Parser.Lib
import           Language.Java.Syntax

-- |
-- provides functions used for checking all sorts of types
--
checkType :: Type -> SearchBehaviour -> Bool
checkType (PrimType prim) behaviour = maybe True (checkPrim prim) (toPrimitive $ command behaviour)
    where
        checkPrim BooleanT "boolean" = True
        checkPrim ByteT "byte" = True
        checkPrim ShortT "short" = True
        checkPrim IntT "int" = True
        checkPrim LongT "long" = True
        checkPrim CharT "char" = True
        checkPrim FloatT "float" = True
        checkPrim DoubleT "double" = True
        checkPrim _ _ = False
checkType (RefType (ArrayType t)) behaviour = checkType t behaviour
checkType (RefType (ClassRefType (ClassType t))) behaviour
    | null t = True
    | otherwise = fromMaybe True $ combine (fmap packageMatch (packageName com)) (fmap classMatch (className com)) (&&)
    where
        classMatch x = identS ( fst $ last t) == x
        packageMatch pkg = not (needsImport behaviour) || pkg == flattenedPackage
        flattenedPackage = concatMap (identS . fst) (init t)
        com = command behaviour

-- | this method maybe returns the primitive used for comparisons against occurences
toPrimitive :: Command -> Maybe String
toPrimitive com
    | isNothing (packageName com) || correctBoxedPrimPackage com = maybePrim
    | otherwise = Nothing
        where
            translateBoxedToPrim "Boolean" = Just "boolean"
            translateBoxedToPrim "Byte" = Just "byte"
            translateBoxedToPrim "Short" = Just "short"
            translateBoxedToPrim "Integer" = Just "int"
            translateBoxedToPrim "Long" = Just "long"
            translateBoxedToPrim "Char" = Just "char"
            translateBoxedToPrim "Float" = Just "float"
            translateBoxedToPrim "Double" = Just "double"
            translateBoxedToPrim _ = Nothing

            translateToPrim name
                | all isLower name = Just name
                | otherwise = translateBoxedToPrim name

            maybePrim = className com >>= translateToPrim

-- | whether we are unable to check a specific primitive type,
-- but the correct package for boxed primitives is given
unspecifiedPrim :: Command -> Bool
unspecifiedPrim com = correctBoxedPrimPackage com && isNothing (className com)

correctBoxedPrimPackage :: Command -> Bool
correctBoxedPrimPackage com = maybe False ("java.lang" ==) (packageName com)

canCheckType :: SearchBehaviour -> Bool
canCheckType behaviour = let com = command behaviour in isJust (packageName com) || isJust (className com)
