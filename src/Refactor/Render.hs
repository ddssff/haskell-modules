{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}

module Refactor.Render
    ( renderModule
    ) where

import Control.Lens (makeLenses, over, set, view)
import Control.Monad.RWS
import Data.Default (Default(def))
import Debug.Trace (trace)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Names (Scoped)
import Language.Haskell.Names.GlobalSymbolTable as Global (Table)
import Prelude hiding (span)
import Refactor.Info (ImportSpecWithDecl, ModuleInfo(..))
import Refactor.Parse (unScope)
import Refactor.SrcLoc

sinf :: (Annotated ast) => ast (Scoped SrcSpanInfo) -> SrcSpanInfo
sinf = unScope . ann

span :: (Annotated ast) => ast (Scoped SrcSpanInfo) -> SrcSpan
span = srcInfoSpan . sinf

spanStart :: (Annotated ast) => ast (Scoped SrcSpanInfo) -> (Int, Int)
spanStart = srcSpanStart . span

spanEnd :: (Annotated ast) => ast (Scoped SrcSpanInfo) -> (Int, Int)
spanEnd = srcSpanEnd . span

data RenderInfo
    = RenderInfo
      { _verbosity :: Int
      , _prefix :: String
      , _label :: String }

instance Default RenderInfo where def = RenderInfo 0 "" ""

$(makeLenses ''RenderInfo)

verbosely :: MonadReader RenderInfo m => Int -> m a -> m a
verbosely level action = local (set verbosity level) action

prefixed :: MonadReader RenderInfo m => String -> m a -> m a
prefixed str action = local (over prefix (++ str)) action

labelled :: MonadReader RenderInfo m => String -> m a -> m a
labelled str action = local (set label str) action

renderModule ::
  forall l. (l ~ SrcSpanInfo)
  => ModuleInfo (Scoped l)
  -> (Decl (Scoped l) -> Bool)
  -> (ExportSpec (Scoped l) -> Bool)
  -> (ImportSpecWithDecl (Scoped l) -> Bool)
  -> (Comment -> Bool)
  -> String
renderModule i@(ModuleInfo {_module = Module _l h ps is ds, _moduleComments = _cs}) selectedDecls selectedExports selectedImports selectedComments =
  snd $ execRWS (pre >> scanPragmas >> scanHeader h >> mapM_ scanImport is >> scanDecls >> post) (def {_verbosity = 0}) (1, 1)
  where
    (SrcSpanInfo (SrcSpan _ msl msc mel mec) _) = _moduleSpan i
    pre = keep (msl, msc)
    scanPragmas = mapM_ (keep . spanEnd) ps
    scanHeader :: Maybe (ModuleHead (Scoped l)) -> RWS RenderInfo String (Int, Int) ()
    scanHeader Nothing = return ()
    scanHeader (Just h'@(ModuleHead _ n w me)) = do
      keep (spanEnd n)
      maybe (return ()) (keep . spanEnd) w
      maybe (return ()) scanExports me
      keep (spanEnd h')

    scanExports :: ExportSpecList (Scoped l) -> RWS RenderInfo String (Int, Int) ()
    scanExports esl@(ExportSpecList _ es) = do
      let si = unScope $ ann esl
      case srcInfoPoints si of
        [] -> keepV "es" (spanStart esl)
        (p : _ps) -> keep (srcSpanStart p)
      _ <- foldM scanExport True es
      keep (spanEnd esl)
    scanExport :: Bool -> ExportSpec (Scoped l) -> RWS RenderInfo String (Int, Int) Bool
    scanExport prev e =
        case selectedExports e of
          True -> do
            (if prev then keep else skip) (spanStart e)
            keep (spanEnd e)
            return True
          False -> do
            (if prev then keep else skip) (spanStart e)
            skip {-"ex"-} (spanEnd e)
            return False

    scanImport :: ImportDecl (Scoped l) -> RWS RenderInfo String (Int, Int) ()
    scanImport idecl@(ImportDecl {importSpecs = Nothing}) = keep (spanEnd idecl)
    scanImport idecl@(ImportDecl {importSpecs = Just (ImportSpecList _ True _)}) = keep (spanEnd idecl)
#if 1
    scanImport idecl@(ImportDecl {importSpecs = Just (ImportSpecList _ False _)}) = keep (spanEnd idecl)
#else
    scanImport idecl@(ImportDecl {importSpecs = Just (ImportSpecList _ False ispecs)}) = do
      _ <- foldM (scanImportSpec idecl) True ispecs
      return ()
    scanImportSpec :: ImportDecl (Scoped l) -> Bool -> ImportSpec (Scoped l) -> RWS RenderInfo String (Int, Int) Bool
    scanImportSpec idecl prev ispec =
        case selectedImports (idecl, ispec) of
          True -> do
            (if prev then keep else {-skip-} keepV "-") (srcSpanStart (srcInfoSpan (ann ispec)))
            keepV "+" (srcSpanEnd (srcInfoSpan (ann ispec)))
            return True
          False -> do
            (if prev then keep else {-skip-} keepV "-") (srcSpanStart (srcInfoSpan (ann ispec)))
            keepV "-" {-skip-} (srcSpanEnd (srcInfoSpan (ann ispec)))
            return False
#endif
      -- keepS (ann i)
      -- keepEV "(i)" (ann i)
{-
    scanImport i@(ImportDecl a m q src safe pkg as (Just specs)) =
      mapM_ scanImportSpec specs >> keepE (ann i)
    scanImportSpec spec = keepE spec
      -- keep (ann a) >> keep (ann m) >> keep (ann q) >> fmap (keep . ann)
-}
    scanDecls = foldM scanDecl True ds

    scanDecl :: Bool -> Decl (Scoped l) -> RWS RenderInfo String (Int, Int) Bool
    scanDecl prev d =
        case selectedDecls d of
          True -> do
            (if prev then keep else skip) (spanStart d)
            keep (spanEnd d)
            return True
          False -> do
            (if prev then keep else skip) (spanStart d)
            skip (spanEnd d)
            return False
{-
        mapM_ (\d -> case selectedDecls d of
                       False -> skipV "d" (srcSpanEnd (srcInfoSpan (ann d)))
                       True -> keepV "7" (srcSpanEnd (srcInfoSpan (ann d)))) ds
-}
    post = keep (mel, mec)

    skip :: (Int, Int) -> RWS RenderInfo String (Int, Int) ()
    skip = put

    skipV :: (Int, Int) -> RWS RenderInfo String (Int, Int) ()
    skipV st = do
      here <- get
      v <- view verbosity
      l <- view label
      when (v > 0 && here /= st) (tell ("[" ++ l ++ " - skipping from " ++ show here ++ " to " ++ show st ++ "]"))
      put st

    keep :: (Int, Int) -> RWS RenderInfo String (Int, Int) ()
    keep (el, ec) = do
      (l', c) <- get
      tell (textOfSpan (SrcSpan "" l' c el ec) (_moduleText i))
      put (el, ec)

    keepV :: String -> (Int, Int) -> RWS RenderInfo String (Int, Int) ()
    keepV n s = tell ("[" ++ n) >> keep s >> tell "]"

    keep' :: String -> SrcSpan -> RWS RenderInfo String (Int, Int) ()
    keep' n s = do
      labelled "k" $ skip (srcSpanStart s)
      tell ("[" ++ n)
      keep (srcSpanEnd s)
      tell "]"

    keep'' :: SrcSpan -> RWS RenderInfo String (Int, Int) ()
    keep'' s = tell "[" >> keep (srcSpanStart s) >> keep (srcSpanEnd s) >> tell "]"
renderModule _ _ _ _ _ = error "renderModule"
