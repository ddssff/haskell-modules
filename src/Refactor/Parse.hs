-- | Parse a set of modules and annotate them with scoping information.

module Refactor.Parse
    ( parseModule
    , parseAndAnnotateModules
    , addScope
    , unScope
    ) where

import Control.Lens (over, view)
import Data.Generics (everywhere, mkT)
import Language.Haskell.Exts.CPP (parseFileContentsWithCommentsAndCPP)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Parser as Exts (fromParseResult)
import Language.Haskell.Names (annotate, resolve, Scoped(..))
import Language.Haskell.Names.Imports (importTable)
import Language.Haskell.Names.ModuleSymbols (moduleTable)
import Refactor.CPP (cppOptions, defaultParseMode, GHCOpts, turnOffLocations)
import Refactor.Info (ModuleInfo(..))
import Refactor.SrcLoc (fixEnds, fixSpan, mapTopAnnotations, spanOfText)

-- | Load a single module:
--
--     λ> loadModule def (ModuleFilePath (Just "src") "Refactor.hs")
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
                    , _moduleText = text
                    , _moduleSpan = spanOfText path text
                    , _moduleGlobals = mempty }

parseAndAnnotateModules :: GHCOpts -> [(FilePath, String)] -> IO [ModuleInfo (Scoped SrcSpanInfo)]
parseAndAnnotateModules opts pairs = addScope <$> mapM (uncurry (parseModule opts)) pairs

addScope :: [ModuleInfo SrcSpanInfo] -> [ModuleInfo (Scoped SrcSpanInfo)]
addScope mods =
    fmap (\m -> m {_module = annotate env (_module m),
                   _moduleGlobals = moduleTable (importTable env (_module m)) (_module m)}) mods
    where env = resolve (fmap _module mods) mempty

unScope :: Scoped a -> a
unScope (Scoped _ l) = l
