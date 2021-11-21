{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE EmptyCase #-}
#endif
module Test.Hspec.Core.Formatters.Pretty.Parser (
  Expression(..)
, Literal(..)
, parseExpression
, unsafeParseExpression
) where

import           Prelude ()
import           Test.Hspec.Core.Compat hiding (fail)
import           Test.Hspec.Core.Formatters.Pretty.Parser.Types

#if __GLASGOW_HASKELL__ < 802 || __GLASGOW_HASKELL__ > 902

parseExpression :: String -> Maybe Expression
parseExpression _ = Nothing

unsafeParseExpression :: String -> Maybe Expression
unsafeParseExpression _ = Nothing

#else

import           GHC.Stack
import           GHC.Exception (throw, errorCallWithCallStackException)

#if __GLASGOW_HASKELL__ >= 804
import           GHC.LanguageExtensions.Type
#endif

#if __GLASGOW_HASKELL__ >= 902
import           GHC.Types.SourceText
#elif __GLASGOW_HASKELL__ >= 900
import           GHC.Types.Basic
import           GHC.Unit.Types
#endif

#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.Parser as GHC
import           GHC.Parser.Lexer
import           GHC.Data.StringBuffer
import           GHC.Data.FastString
import           GHC.Types.SrcLoc
import qualified GHC.Data.EnumSet as EnumSet
import           GHC.Types.Name
import           GHC.Types.Name.Reader
import           GHC.Parser.PostProcess hiding (Tuple)
#else
import           Lexer
import qualified Parser as GHC
import           StringBuffer
import           FastString
import           SrcLoc
import           Name
import           RdrName
import           BasicTypes
import           Module
#if __GLASGOW_HASKELL__ >= 804
import qualified EnumSet
#endif
#endif

#if __GLASGOW_HASKELL__ == 810
import           RdrHsSyn hiding (Tuple)
#endif

#if __GLASGOW_HASKELL__ >= 810
import           GHC.Hs
#else
import           HsSyn
#endif

#if __GLASGOW_HASKELL__ <= 806
import           Data.Bits
import           Control.Exception
#endif

parseExpression :: String -> Maybe Expression
parseExpression = parseWith (const Nothing)

unsafeParseExpression :: String -> Maybe Expression
unsafeParseExpression = parseWith throwError

parseWith :: (Error -> Maybe Expression) -> String -> Maybe Expression
parseWith err = parse >=> either err Just . toExpression

data Error = Error CallStack String

throwError :: Error -> a
throwError (Error stack err) = throw $ errorCallWithCallStackException err stack

fail :: HasCallStack => String -> Either Error a
fail = Left . Error callStack

class ToExpression a where
  toExpression :: a -> Either Error Expression

#if __GLASGOW_HASKELL__ < 806
#define _x
#endif

#if __GLASGOW_HASKELL__ >= 900
#define X(name, expr)
#elif __GLASGOW_HASKELL__ == 810
#define X(name, expr) name none -> case none of
#elif __GLASGOW_HASKELL__ >= 806
#define X(name, expr) name none -> case none of NoExt -> expr
#else
#define X(name, expr)
#endif

#if __GLASGOW_HASKELL__ >= 804
#define GhcPsHsLit GhcPs
#else
type GhcPs = RdrName
#define GhcPsHsLit
#endif

#if __GLASGOW_HASKELL__ >= 902
#define _listSynExpr
#endif

#if __GLASGOW_HASKELL__ >= 806
#define RecCon(name, fields) RecordCon _ (L _ name) fields
#else
#define RecCon(name, fields) RecordCon (L _ name) _ _ fields
#endif

#define REJECT(name) name{} -> fail "name"

instance ToExpression (HsExpr GhcPs) where
  toExpression expr = case expr of
    HsVar _x name -> toExpression name
    HsLit _x lit -> toExpression lit
    HsOverLit _x lit -> toExpression lit
    HsApp _x f x -> App <$> toExpression f <*> toExpression x
    NegApp _x e _ -> toExpression e >>= \ x -> case x of
      Literal (Rational n) -> return $ Literal (Rational $ negate n)
      Literal (Integer n) -> return $ Literal (Integer $ negate n)
      _ -> fail "NegApp"
    HsPar _x e -> Parentheses <$> toExpression e
    ExplicitTuple _x xs _ -> Tuple <$> mapM toExpression xs
    ExplicitList _ _listSynExpr xs -> List <$> mapM toExpression xs
    RecCon(name, fields) -> Record (showRdrName name) <$> (recordFields $ rec_flds fields)
      where
        fieldName = showFieldLabel . unLoc . hsRecFieldLbl
        recordFields = mapM (recordField . unLoc)
        recordField field = (,) (fieldName field) <$> toExpression (hsRecFieldArg field)

    REJECT(HsUnboundVar)
    REJECT(HsConLikeOut)
    REJECT(HsRecFld)
    REJECT(HsOverLabel)
    REJECT(HsIPVar)
    REJECT(HsLam)
    REJECT(HsLamCase)
    REJECT(HsAppType)
    REJECT(OpApp)
    REJECT(SectionL)
    REJECT(SectionR)
    REJECT(ExplicitSum)
    REJECT(HsCase)
    REJECT(HsIf)
    REJECT(HsMultiIf)
    REJECT(HsLet)
    REJECT(HsDo)
    REJECT(RecordUpd)
    REJECT(ExprWithTySig)
    REJECT(ArithSeq)
    REJECT(HsBracket)
    REJECT(HsRnBracketOut)
    REJECT(HsTcBracketOut)
    REJECT(HsSpliceE)
    REJECT(HsProc)
    REJECT(HsStatic)
    REJECT(HsTick)
    REJECT(HsBinTick)
#if __GLASGOW_HASKELL__ >= 902
    REJECT(HsGetField)
    REJECT(HsProjection)
#endif
#if __GLASGOW_HASKELL__ >= 900
    REJECT(HsPragE)
#endif
#if __GLASGOW_HASKELL__ <= 810
    REJECT(HsSCC)
    REJECT(HsCoreAnn)
    REJECT(HsTickPragma)
    REJECT(HsWrap)
#endif
#if __GLASGOW_HASKELL__ <= 808
    REJECT(HsArrApp)
    REJECT(HsArrForm)
    REJECT(EWildPat)
    REJECT(EAsPat)
    REJECT(EViewPat)
    REJECT(ELazyPat)
#endif
#if __GLASGOW_HASKELL__ <= 804
    REJECT(HsAppTypeOut)
    REJECT(ExplicitPArr)
    REJECT(ExprWithTySigOut)
    REJECT(PArrSeq)
#endif
    X(XExpr, fail "XExpr")

instance ToExpression RdrName where
  toExpression = return . Id . showRdrName

instance ToExpression (HsTupArg GhcPs) where
  toExpression t = case t of
    Present _x expr -> toExpression expr
    Missing _ -> fail "Missing (tuple section)"
    X(XTupArg, fail "XTupArg")

instance ToExpression e => ToExpression (GenLocated l e) where
  toExpression (L _ e) = toExpression e

instance ToExpression (HsOverLit GhcPs) where
  toExpression = toExpression . ol_val

#if __GLASGOW_HASKELL__ > 802
#define _integralSource

instance ToExpression IntegralLit where
  toExpression il = toExpression (il_value il)
#endif

instance ToExpression OverLitVal where
  toExpression lit = case lit of
    HsIntegral _integralSource il -> toExpression il
    HsFractional fl -> toExpression fl
    HsIsString _ str -> toExpression str

instance ToExpression FractionalLit where
  toExpression fl = toExpression (fl_value fl)

#if __GLASGOW_HASKELL__ >= 902
fl_value :: FractionalLit -> Rational
fl_value = rationalFromFractionalLit
#endif

instance ToExpression FastString where
  toExpression = return . Literal . String . unpackFS

instance ToExpression Integer where
  toExpression = return . Literal . Integer

instance ToExpression Rational where
  toExpression = return . Literal . Rational

instance ToExpression Char where
  toExpression = return . Literal . Char

instance ToExpression (HsLit GhcPsHsLit) where
  toExpression lit = case lit of
    HsChar _ c -> toExpression c
    HsString _ str -> toExpression str
    REJECT(HsCharPrim)
    REJECT(HsStringPrim)
    REJECT(HsInt)
    REJECT(HsIntPrim)
    REJECT(HsWordPrim)
    REJECT(HsInt64Prim)
    REJECT(HsWord64Prim)
    REJECT(HsInteger)
    REJECT(HsRat)
    REJECT(HsFloatPrim)
    REJECT(HsDoublePrim)
    X(XLit, fail "XLit")

showFieldLabel :: FieldOcc GhcPs -> String
showFieldLabel label = case label of
#if __GLASGOW_HASKELL__ >= 806
  FieldOcc _ (L _ name) -> showRdrName name
#else
  FieldOcc (L _ name) _ -> showRdrName name
#endif
  X(XFieldOcc, "")

showRdrName :: RdrName -> String
showRdrName n = case n of
  Unqual name -> showOccName name
  Qual _ name -> showOccName name
  Orig _ name -> showOccName name
  Exact name -> showOccName (nameOccName name)

showOccName :: OccName -> String
showOccName = unpackFS . occNameFS

parse :: String -> Maybe (HsExpr GhcPs)
parse input = case runParser input pHsExpr of
  POk _ (L _ x) -> Just x
  PFailed {} -> Nothing
  where
    pHsExpr = do
      r <- GHC.parseExpression
      runPV (unECP r)

#if __GLASGOW_HASKELL__ <= 900
#if __GLASGOW_HASKELL__ >= 810
    unECP = runECP_PV
#else
    unECP = return
    runPV = id
#endif
#endif

runParser :: String -> P a -> ParseResult a
runParser str parser = unP parser parseState
  where
    location = mkRealSrcLoc "" 1 1
    input = stringToStringBuffer str
    parseState = initParserState opts input location
    opts = mkParserOpts warn extensions False False False True

#if __GLASGOW_HASKELL__ >= 804
    extensions = EnumSet.fromList [TraditionalRecordSyntax]
    warn = EnumSet.empty
#else
    extensions = mempty
    warn = mempty
#endif

#if __GLASGOW_HASKELL__ <= 900
    initParserState = mkPStatePure
    mkParserOpts warningFlags extensionFlags = mkParserFlags' warningFlags extensionFlags unit
#if __GLASGOW_HASKELL__ == 900
    unit = UnitId ""
#else
    unit = fsToUnitId ""
#endif
#endif

#if __GLASGOW_HASKELL__ <= 806
    mkParserFlags' ws es u _ _ _ _ = assert (traditionalRecordSyntaxEnabled extensionsBitmap) $
      ParserFlags ws es u extensionsBitmap
    extensionsBitmap = shift 1 traditionalRecordSyntaxBit
#if __GLASGOW_HASKELL__ == 806
    traditionalRecordSyntaxBit = 28
#else
    traditionalRecordSyntaxBit = 29
#endif
#endif

#endif
