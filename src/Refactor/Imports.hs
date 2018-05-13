{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module Refactor.Imports
    ( buildEnvironment
    ) where

import Data.Set as Set (difference, Set, toList)
import Language.Haskell.Exts.Syntax (ImportDecl(..), Module, ModuleName(..))
import Language.Haskell.Names.SyntaxUtils (dropAnn, getImports, getModuleName)
import Language.Haskell.TH (ExpQ, Info, listE, Name, tupE)
import Language.Haskell.TH.Lift (lift)
import Refactor.Reify (findModuleSymbols)
import Refactor.Utils (mapFromList, setFromList)

-- | Build an environment that contains all the modules imported
-- by any of the modules in the list.
buildEnvironment ::
    forall l l'. Ord l
    => Int
    -> (Name -> Maybe Info)
    -> [ModuleName l'] -- ^ Additional module names to load, e.g. ["Prelude"]
    -> [Module l] -- ^ Parsed module list - do not run findModuleSymbols on these
    -> ExpQ -- Q Environment
buildEnvironment verbosity special extra ms =
    [|mapFromList $(listE (fmap (\name -> tupE [lift name, findModuleSymbols verbosity special (getModuleNameString name)]) (Set.toList mods)))|]
    -- [|foldr (\mp m -> Map.insert m $(findModuleSymbols s) mp) mods|]
    -- [|Map.fromList $(listE (fmap (\m@(ModuleName _ s) -> [|($(lift m), $(findModuleSymbols s))|]) mods))|]
    where
      mods :: Set (ModuleName ())
      mods = Set.difference (setFromList (fmap getImportModName (fmap dropAnn ims) ++ fmap dropAnn extra)) pms
      ims :: [ImportDecl l]
      ims = concatMap getImports ms
      pms :: Set (ModuleName ())
      pms = setFromList (fmap (dropAnn . getModuleName) ms)

getImportModName :: ImportDecl l -> ModuleName l
getImportModName (ImportDecl _ m _ _ _ _ _ _) = m

getModuleNameString :: ModuleName l -> String
getModuleNameString (ModuleName _ s) = s

-- | Build a predicate that removes unused imports.
-- cleanImports :: Environment -> ModuleInfo (Scoped l) -> 
