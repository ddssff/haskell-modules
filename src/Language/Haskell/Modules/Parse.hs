-- | Parse a set of modules and annotate them with scoping information.

{-# LANGUAGE FlexibleContexts #-}

module Language.Haskell.Modules.Parse
    ( parseModule
    , parseAndAnnotateModules
    , addScope
    , unScope
    ) where

import Control.Lens (over, view)
import Control.Monad.State (get, modify, MonadState)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Generics (everywhere, mkT)
import Language.Haskell.Exts.CPP (parseFileContentsWithCommentsAndCPP)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Parser as Exts (fromParseResult)
import Language.Haskell.Modules.CPP (cppOptions, defaultParseMode, GHCOpts, turnOffLocations)
import Language.Haskell.Modules.Info (ModuleInfo(..))
import Language.Haskell.Modules.SrcLoc (fixEnds, fixSpan, mapTopAnnotations, spanOfText)
import Language.Haskell.Names (annotate, Environment, resolve, Scoped(..))

-- | Load a single module:
--
--     λ> loadModule def "src" "Language/Haskell/Refactor/Utils.hs"
--     ModuleInfo {_moduleKey = ModuleKey {_moduleTop = "/home/dsf/git/refact-global-hse/src" ... }}
--
-- This is an IO operation because 'parseFileContentsWithCommentsAndCPP'
-- is IO, and that is IO because of the fact that cpp does #includes.
-- The path is not opened and read, it is just used as a name.
parseModule :: GHCOpts -> FilePath -> String -> IO (ModuleInfo SrcSpanInfo)
parseModule opts path text = do
  let mode = defaultParseMode opts path
  let opts' = opts -- foldr applyHashDefine opts (view hashDefines opts)
      opts'' = over cppOptions turnOffLocations opts'
  (parsed', comments) <- Exts.fromParseResult <$> parseFileContentsWithCommentsAndCPP (view cppOptions opts'') mode text
  let parsed = mapTopAnnotations (fixEnds comments text) $ everywhere (mkT fixSpan) parsed'
  pure $ ModuleInfo { _module = parsed
                    , _moduleComments = comments
                    , _modulePath = Just path
                    , _moduleText = text
                    , _moduleSpan = spanOfText path text }

parseAndAnnotateModules ::
    (MonadState Environment m, MonadIO m)
    => GHCOpts
    -> [(FilePath, String)]
    -> m [ModuleInfo (Scoped SrcSpanInfo)]
parseAndAnnotateModules opts pairs =
    addScope =<< mapM (liftIO . uncurry (parseModule opts)) pairs

addScope :: MonadState Environment m => [ModuleInfo SrcSpanInfo] -> m [ModuleInfo (Scoped SrcSpanInfo)]
addScope mods = do
  modify (resolve (fmap _module mods))
  env <- get
  return $ fmap (\m -> m {_module = annotate env (_module m)}) mods

unScope :: Scoped a -> a
unScope (Scoped _ l) = l
