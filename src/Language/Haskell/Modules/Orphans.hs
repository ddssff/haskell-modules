{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Haskell.Modules.Orphans () where

import Data.Default (Default(def))
import Data.Graph.Inductive as G
import Data.Monoid ((<>))
import Language.Haskell.Exts.SrcLoc
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

instance Pretty SrcLoc where
    pPrint l = text ("(l" <> show (srcLine l) ++ ",c" ++ show (srcColumn l) ++ ")")

instance Pretty SrcSpan where
    pPrint (SrcSpan _ bl bc el ec) = text ("(l" <> show bl ++ ",c" ++ show bc ++ ")->" ++
                                           "(l" <> show el ++ ",c" ++ show ec ++ ")")

instance Pretty SrcSpanInfo where
    pPrint = pPrint . srcInfoSpan

instance Default (G.Gr a b) where
  def = G.empty
