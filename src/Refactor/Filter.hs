{-# LANGUAGE RankNTypes #-}

module Refactor.Filter
    ( filterModule
    ) where

import Data.Generics
import Language.Haskell.Exts
import Language.Haskell.Names
import Refactor.Info (ImportSpecWithDecl, ModuleInfo(..))

-- | Given a module, generate a new module which only has the given
-- declarations, assumed to be a subset of the declarations from the
-- original.  Besides updating the declaration list, this must filter
-- the exports and strip out comments associated with deleted decls or
-- exports.
filterModule ::
    forall l. (Data l, Ord l, Show l)
    => ModuleInfo (Scoped l)
    -> (Decl (Scoped l) -> Bool)
    -> (ExportSpec (Scoped l) -> Bool)
    -> (ImportSpecWithDecl (Scoped l) -> Bool)
    -> (Comment -> Bool)
    -> ModuleInfo (Scoped l)
filterModule i@(ModuleInfo {_module = Module l h ps is ds, _moduleComments = cs})
               selectedDecls selectedExports selectedImports selectedComments =
    i { _module = Module l (fmap doHead h) ps is (filter selectedDecls ds),
        _moduleComments = filter (selectedComments) cs}
    where doHead (ModuleHead l' n w me) = ModuleHead l' n w (fmap doSpecs me)
          doSpecs (ExportSpecList l' es) = ExportSpecList l' (filter selectedExports es)
filterModule i@(ModuleInfo {_module = _}) _ _ _ _ = i
