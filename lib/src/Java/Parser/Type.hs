module Java.Parser.Typechecker where

import           Data.List
import           Data.List.Split
import           Java.Parser.Lib
import           Java.Parser.Usagefinder
import           Language.Java.Parser
import           Language.Java.Syntax


checkType :: Type -> SearchBehaviour -> Bool
checkType (PrimType prim) behaviour = fromMaybe True map (checkPrim prim) toPrimitive command behaviour
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
    | otherwise = fromMaybe True (&&) <$> map packageMatch packageName command behaviour <*> map classMatch className command behaviour
    where
        classMatch x = fst (last t) == x
        packageMatch pkg = not (needsImport behaviour) || pkg == flattenedPackage
        flattenedPackage = fold (++) map fst "" init t

-- | this method maybe returns the primitive used for comparisons against occurences
toPrimitive :: Command -> Maybe String
tPrimitive com
    | isNothing packageName com || correctBoxedPrimPackage com = maybePrim
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

            translateToPrim className
                | all isLower className = Just className
                | othweise = translateBoxedToPrim className

            maybePrim = className com >>= translateToPrim

-- | whether we are unable to check a specific primitive type,
-- but the correct package for boxed primitives is given
unspecifiedPrim :: Command -> Bool
unspecifiedPrim com = correctBoxedPrimPackage com && isNothing className com

correctBoxedPrimPackage :: Command -> Bool
correctBoxedPrimPackage = fromMaybe False map (== "java.lang") packageName

canCheckType :: SearchBehaviour -> Bool
canCheckType behaviour = let com = command behaviour in isJust . packageName com || isJust . className com

