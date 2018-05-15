-- | Turn modules with added and removed elements back into text.

{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Language.Haskell.Modules.Render
    ( renderModule
    ) where

import Control.Lens (makeLenses, over, set, view)
import Control.Monad.RWS
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Modules.Info (ImportSpecWithDecl, ModuleInfo(..))
import Language.Haskell.Modules.Parse (unScope)
import Language.Haskell.Modules.SrcLoc
import Language.Haskell.Names (Scoped)
import Prelude hiding (span)

-- | State information for the renderer.
data RenderInfo l
    = RenderInfo
      { _moduleInfo :: ModuleInfo (Scoped l)
      , _verbosity :: Int
      , _prefix :: String
      , _label :: String }

$(makeLenses ''RenderInfo)

initRenderInfo :: ModuleInfo (Scoped l) -> RenderInfo l
initRenderInfo i = RenderInfo i 0 "" ""

verbosely :: MonadReader (RenderInfo l) m => Int -> m a -> m a
verbosely level action = local (set verbosity level) action

prefixed :: MonadReader (RenderInfo l) m => String -> m a -> m a
prefixed str action = local (over prefix (++ str)) action

labelled :: MonadReader (RenderInfo l) m => String -> m a -> m a
labelled str action = local (set label str) action

-- | Render the module, applying these predicates to filter the
-- module's elements and adjusting the whitespace and other formatting
-- so that the result is as close to a "desirable" format as possible.
renderModule ::
  forall l. (l ~ SrcSpanInfo)
  => ModuleInfo (Scoped l)
  -> (Decl (Scoped l) -> Bool)
  -> (ExportSpec (Scoped l) -> Bool)
  -> (ImportSpecWithDecl (Scoped l) -> Bool)
  -> String
renderModule i@(ModuleInfo {_module = Module _l h ps is ds, _moduleComments = _cs})
             selectedDecls selectedExports selectedImports =
  snd $ execRWS (pre >> scanPragmas >> scanHeader h >> mapM_ scanImport is >> scanDecls >> post)
          ((initRenderInfo i) {_verbosity = 0}) (1, 1)
  where
    (SrcSpanInfo (SrcSpan _ msl msc mel mec) _) = _moduleSpan i
    -- keep everything up to the start of the overall module span - probably nothing?
    pre = keep (msl, msc)
    -- Keep all the pragmas
    scanPragmas = mapM_ (keep . spanEnd) ps

    scanHeader :: Maybe (ModuleHead (Scoped l)) -> RWS (RenderInfo l) String (Int, Int) ()
    -- If there is no "module" declaration continue to the imports
    scanHeader Nothing = return ()
    -- If there is, continue to the exports and scan them
    scanHeader (Just h'@(ModuleHead _ n w me)) = do
      keep (spanEnd n)
      maybe (return ()) (keep . spanEnd) w -- continue to the export list
      maybe (return ()) scanExports me
      keep (spanEnd h')

    scanExports :: ExportSpecList (Scoped l) -> RWS (RenderInfo l) String (Int, Int) ()
    scanExports esl@(ExportSpecList _ es) = do
      -- Keep the text between the start of the import list and the
      -- start of the first export.  Then scan each exports.
      case srcInfoPoints (sinf esl) of
        [] -> keep (spanStart esl)
        (p : _ps) -> keep (srcSpanStart p)
      _ <- foldM scanExport True es
      keep (spanEnd esl)

    scanExport :: Bool -> ExportSpec (Scoped l) -> RWS (RenderInfo l) String (Int, Int) Bool
    scanExport prev e =
        -- This case handles the text between two imports depending on
        -- whether the previous one was kept or omitted, and whether
        -- the current one is to be kept or omitted.  FIXME: this will
        -- often leave a trailing comma at the end of the filtered
        -- export list, which (surprisingly) is accepted by GHC.
        case selectedExports e of
          True -> do
            (if prev then keep else skip) (spanStart e)
            keep (spanEnd e)
            return True
          False -> do
            (if prev then keep else skip) (spanStart e)
            skip (spanEnd e)
            return False

    -- Imports have two parts - a decl and zero or more specs.  The
    -- filtering is done based on (decl, spec) pairs.
    scanImport :: ImportDecl (Scoped l) -> RWS (RenderInfo l) String (Int, Int) ()
    -- An unqualified module import
    scanImport idecl@(ImportDecl {importSpecs = Nothing}) = keep (spanEnd idecl)
    -- An import hiding
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

    scanDecl :: Bool -> Decl (Scoped l) -> RWS (RenderInfo l) String (Int, Int) Bool
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
renderModule _ _ _ _ = error "renderModule"

skip :: (Int, Int) -> RWS (RenderInfo l) String (Int, Int) ()
skip = put

skipV :: (Int, Int) -> RWS (RenderInfo l) String (Int, Int) ()
skipV st = do
  here <- get
  v <- view verbosity
  l <- view label
  when (v > 0 && here /= st) (tell ("[" ++ l ++ " - skipping from " ++ show here ++ " to " ++ show st ++ "]"))
  put st

keep :: (Int, Int) -> RWS (RenderInfo l) String (Int, Int) ()
keep (el, ec) = do
  i <- view moduleInfo
  (l', c) <- get
  tell (textOfSpan (SrcSpan "" l' c el ec) (_moduleText i))
  put (el, ec)

keepV :: String -> (Int, Int) -> RWS (RenderInfo l) String (Int, Int) ()
keepV n s = tell ("[" ++ n) >> keep s >> tell "]"

keep' :: String -> SrcSpan -> RWS (RenderInfo l) String (Int, Int) ()
keep' n s = do
  labelled "k" $ skip (srcSpanStart s)
  tell ("[" ++ n)
  keep (srcSpanEnd s)
  tell "]"

keep'' :: SrcSpan -> RWS (RenderInfo l) String (Int, Int) ()
keep'' s = tell "[" >> keep (srcSpanStart s) >> keep (srcSpanEnd s) >> tell "]"

-- | Get a 'SrcSpanInfo' from the annotation.
sinf :: (Annotated ast) => ast (Scoped SrcSpanInfo) -> SrcSpanInfo
sinf = unScope . ann

-- | Get a 'SrcSpan' from the annotation.
span :: (Annotated ast) => ast (Scoped SrcSpanInfo) -> SrcSpan
span = srcInfoSpan . sinf

-- | Get the 'SrcSpan' start point from the annotation.
spanStart :: (Annotated ast) => ast (Scoped SrcSpanInfo) -> (Int, Int)
spanStart = srcSpanStart . span

-- | Get the 'SrcSpan' end point from the annotation.
spanEnd :: (Annotated ast) => ast (Scoped SrcSpanInfo) -> (Int, Int)
spanEnd = srcSpanEnd . span
