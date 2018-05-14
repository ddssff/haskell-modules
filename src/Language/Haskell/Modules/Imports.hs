{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module Language.Haskell.Modules.Imports
    ( buildEnvironment
    ) where

import Data.Set as Set (difference, Set, toList)
import Language.Haskell.Exts.Syntax (ImportDecl(..), Module, ModuleName(..))
import Language.Haskell.Modules.Reify (findModuleSymbols)
import Language.Haskell.Modules.Utils (mapFromList, setFromList)
import Language.Haskell.Names.SyntaxUtils (dropAnn, getImports, getModuleName)
import Language.Haskell.TH (ExpQ, Info, listE, Name, tupE)
import Language.Haskell.TH.Lift (lift)

-- | The Environment is the thing that imports reach out to.  This
-- builds an environment that contains all the modules imported by any
-- of the modules in the list.
buildEnvironment ::
       Int -- ^ Verbosity - if you encounter a dangerous name that your dangerous name handler
           -- doesn't know, set this to 1 to get a trace of the symbols being reified.
    -> (Name -> Maybe Info) -- ^ Dangerous name handler
    -> Set (ModuleName ()) -- ^ Use 'importedModules' to convert things to this set of imported modules
    -> ExpQ -- Q Environment
buildEnvironment verbosity danger mnames =
    [|mapFromList $(listE (fmap (\name -> tupE [lift name,
                                                findModuleSymbols verbosity danger (getModuleNameString name)])
                             (Set.toList mnames)))|]
{-
    where
      mods :: Set (ModuleName ())
      mods = Set.difference (setFromList (fmap getImportModName (fmap dropAnn ims) ++ fmap dropAnn extra)) pms
      ims :: [ImportDecl l]
      ims = concatMap getImports ms
      pms :: Set (ModuleName ())
      pms = setFromList (fmap (dropAnn . getModuleName) ms)
-}

getImportModName :: ImportDecl l -> ModuleName l
getImportModName (ImportDecl _ m _ _ _ _ _ _) = m

getModuleNameString :: ModuleName l -> String
getModuleNameString (ModuleName _ s) = s

-- | Build a predicate that removes unused imports.
-- cleanImports :: Environment -> ModuleInfo (Scoped l) -> 
