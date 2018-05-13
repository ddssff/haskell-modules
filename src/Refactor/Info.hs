{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Refactor.Info
    ( ModuleInfo(..)
    , ImportSpecWithDecl
    ) where

import Data.Generics
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Names.GlobalSymbolTable as Global (Table)

data ModuleInfo l =
    ModuleInfo { _module :: Module l
               , _moduleComments :: [Comment]
               -- , _modulePath :: FilePath
               , _moduleText :: String
               , _moduleSpan :: SrcSpanInfo
               , _moduleGlobals :: Global.Table
               } deriving (Data, Typeable, Functor, Show)

type ImportSpecWithDecl l = (ImportDecl l, ImportSpec l)
