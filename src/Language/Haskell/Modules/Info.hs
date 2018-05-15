{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall #-}

module Language.Haskell.Modules.Info
    ( ModuleInfo(..)
    , ImportSpecWithDecl
    ) where

import Data.List (nub)
import Data.Generics (Data, Typeable)
import Instances.TH.Lift ()
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax (Module, ImportDecl, ImportSpec)
import Language.Haskell.Modules.Orphans ()
import Language.Haskell.Names.GlobalSymbolTable as Global (Table)
import Language.Haskell.TH.Lift (deriveLift)

data ModuleInfo l =
    ModuleInfo { _module :: Module l
               , _moduleComments :: [Comment]
               -- , _modulePath :: FilePath
               , _moduleText :: String
               , _moduleSpan :: SrcSpanInfo
               , _moduleGlobals :: Global.Table
               } deriving (Data, Typeable, Functor, Show)

type ImportSpecWithDecl l = (ImportDecl l, ImportSpec l)

$(deriveLift ''ModuleInfo)
