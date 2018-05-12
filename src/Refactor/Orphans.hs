{-# LANGUAGE TemplateHaskell #-}

module Refactor.Orphans () where

import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Language.Haskell.Exts as Exts (ModuleName)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax (Name)
import Language.Haskell.Interpreter
import Language.Haskell.Names (Symbol)
import Language.Haskell.TH.Lift
import Data.Monoid ((<>))
import Instances.TH.Lift

instance Pretty SrcLoc where
    pPrint l = text ("(l" <> show (srcLine l) ++ ",c" ++ show (srcColumn l) ++ ")")

instance Pretty SrcSpan where
    pPrint (SrcSpan _ bl bc el ec) = text ("(l" <> show bl ++ ",c" ++ show bc ++ ")->" ++
                                           "(l" <> show el ++ ",c" ++ show ec ++ ")")

instance Pretty SrcSpanInfo where
    pPrint = pPrint . srcInfoSpan

$(deriveLiftMany [''ModuleElem, ''Exts.ModuleName, ''Symbol, ''Name])
