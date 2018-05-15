-- | Build and use graphs of module properties.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall -ddump-minimal-imports #-}

module Language.Haskell.Modules.Graphs
    ( declGroupName
    , DeclGroup(unDecs)
    , withUsesGraph
    ) where

import Control.Monad.State ( State )
import Data.Generics ( Data )
import Data.Graph.Inductive ( Gr, NodeMap )
import Data.Maybe ( mapMaybe )
import Data.Set as Set ( Set, unions, union, toList, null, intersection, fromList, empty )
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo )
import Language.Haskell.Exts.Syntax ( Decl(TypeSig) )
import Language.Haskell.Modules.FGL ( runGraph, mkGraphM )
import Language.Haskell.Modules.Info ( ModuleInfo(_module), moduleGlobals )
import Language.Haskell.Modules.Query ( declares, uses )
import Language.Haskell.Names ( Environment, Scoped(..), Symbol )
import Language.Haskell.Names.GlobalSymbolTable ()
import Language.Haskell.Names.ModuleSymbols ( getTopDeclSymbols )
import Language.Haskell.Names.SyntaxUtils ( getModuleName, getModuleDecls )

-- | A group of declarations that should not be split up - e.g. a
-- signature and the corresponding declaration(s).
newtype DeclGroup l = DeclGroup {unDecs :: [Decl l]} deriving (Data, Eq, Ord, Show, Functor)

declGroupName :: (Data l, Ord l) => Environment -> ModuleInfo l -> DeclGroup l -> Symbol
declGroupName env i (DeclGroup ds) =
    case Set.toList (foldr1 Set.intersection (fmap (declares env i) ds)) of
      [s] -> s
      [] -> error "declGroupName 1"
      _ -> error "declGroupName 2"

-- | Declarations come in sets - a signature, followed by one or more
-- Decls.
groupDecs :: forall l. (Data l, Ord l, Show l) => Environment -> ModuleInfo (Scoped l) -> [Decl (Scoped l)] -> [DeclGroup (Scoped l)]
groupDecs _ _ [] = error "makeDecs - invalid argument"
groupDecs env i (d1 : ds1) =
    -- With foldr we encounter the declarations in reverse, so the
    -- signature ends up with the previous declaration.
    snd $ foldl go (Set.fromList (getTopDeclSymbols table (getModuleName (_module i)) d1), [DeclGroup [d1]]) ds1
    where
      table = moduleGlobals env (_module i)
      go :: (Set Symbol, [DeclGroup (Scoped l)]) -> Decl (Scoped l) -> (Set Symbol, [DeclGroup (Scoped l)])
      go (_, []) _ = (Set.empty, [])
      go (_ss, DeclGroup ds : more) d@(TypeSig {}) =
        -- Assuming signature comes first - is this a bad assumption?
          (mempty, DeclGroup [d] : DeclGroup ds : more)
      -- If the symbol set is empty we have seen a signature, this must(?)
      -- be the corresponding declaration.
      go (ss, DeclGroup ds : more) d | Set.null ss =
          let ss' = Set.fromList (getTopDeclSymbols table (getModuleName (_module i)) d) in
          (Set.union ss ss', DeclGroup (ds ++ [d]) : more)
      go (ss, DeclGroup ds : more) d =
          let ss' = Set.fromList (getTopDeclSymbols table (getModuleName (_module i)) d) in
          if Set.null (Set.intersection ss ss')
          then (ss', DeclGroup [d] : DeclGroup ds : more)
          else (Set.union ss ss', DeclGroup (ds ++ [d]) : more)

-- | Build a graph whose nodes are declaration groups and whose edges
-- are the "declares, uses" relation.  Each edge is labeled with a set
-- of symbols.
withUsesGraph ::
    forall l r. (l ~ SrcSpanInfo)
    => Environment
    -> ModuleInfo (Scoped l)
    -> (Gr (DeclGroup (Scoped l)) (Set Symbol) -> State (NodeMap (DeclGroup (Scoped l))) r)
    -> (r, NodeMap (DeclGroup (Scoped l)))
withUsesGraph env i f =
    runGraph (mkGraphM declGroups (concatMap (\a -> mapMaybe (edge a) declGroups) declGroups) >>= f)
    where
      declGroups :: [DeclGroup (Scoped l)]
      declGroups = groupDecs env i (getModuleDecls (_module i))
      -- Create edges from any declaration A to any other declaration
      -- B such that A declares a symbol that B uses.
      edge :: DeclGroup (Scoped l) -> DeclGroup (Scoped l) -> Maybe (DeclGroup (Scoped l), DeclGroup (Scoped l), Set Symbol)
      edge a b = if Set.null common then Nothing else Just (a, b, common)
          where common = Set.intersection
                           (Set.unions (fmap (declares env i) (unDecs a)))
                           (Set.unions (fmap (uses (moduleGlobals env (_module i))) (unDecs b)))
