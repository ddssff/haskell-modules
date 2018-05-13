{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}

module Refactor.Orphans () where

import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Language.Haskell.Exts.SrcLoc
import Data.Monoid ((<>))

instance Pretty SrcLoc where
    pPrint l = text ("(l" <> show (srcLine l) ++ ",c" ++ show (srcColumn l) ++ ")")

instance Pretty SrcSpan where
    pPrint (SrcSpan _ bl bc el ec) = text ("(l" <> show bl ++ ",c" ++ show bc ++ ")->" ++
                                           "(l" <> show el ++ ",c" ++ show ec ++ ")")

instance Pretty SrcSpanInfo where
    pPrint = pPrint . srcInfoSpan
