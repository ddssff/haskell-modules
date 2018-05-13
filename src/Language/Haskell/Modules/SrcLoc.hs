-- | Utility functions for the haskell-src-exts type SrcLoc.

{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, PackageImports, ScopedTypeVariables, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Language.Haskell.Modules.SrcLoc
    ( -- * SpanInfo queries
--      EndLoc(endLoc)
--    -- * Location and span info for a piece of text
      spanOfText
    , endLocOfText
--    -- * Split text at a location
    , splitText
--    , splits
--    , splits'
--    -- * Use span info to extract text
--    , textTripleOfSpan
    , textOfSpan
--
--    , testSpan
    , fixSpan
    , fixEnds
--
--    , locSum
--    , locDiff
--
--    , endOfPragmas
--    , endOfHeader
--    , endOfImports
--    , endOfImportSpecs
--    , endOfDecls
--
    , mapTopAnnotations
    ) where

import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Modules.Utils (lines')
import Language.Haskell.Names

locDiff :: SrcLoc -> SrcLoc -> SrcLoc
locDiff (SrcLoc file l1 c1) (SrcLoc _ l2 c2) =
    if l1 == l2
    then SrcLoc file 1 (c1 - c2 + 1)
    else SrcLoc file (l1 - l2 + 1) c1

spanDiff :: SrcSpan -> SrcLoc -> SrcSpan
spanDiff sp l = mkSrcSpan (locDiff (getPointLoc sp) l) (locDiff (endLoc sp) l)

locSum :: SrcLoc -> SrcLoc -> SrcLoc
locSum (SrcLoc f l1 c1) (SrcLoc _ l2 c2) =
    if l2 == 1
    then SrcLoc f (l1 + l2 - 1) (c1 + c2 - 1)
    else SrcLoc f (l1 + l2 - 1) c2

endLocOfText :: FilePath -> String -> SrcLoc
endLocOfText path x =
    case ls of
      [] -> SrcLoc {srcFilename = path, srcLine = 1, srcColumn = 1}
      _ -> SrcLoc {srcFilename = path, srcLine = length ls, srcColumn = length (last ls) + 1}
    where ls = lines' x

-- | Return a span that exactly covers the string s
spanOfText :: FilePath -> String -> SrcSpanInfo
spanOfText path s =
    let end = endLocOfText path s in
    SrcSpanInfo {srcInfoSpan = mkSrcSpan (SrcLoc path 1 1) (SrcLoc path (srcLine end) (srcColumn end)),
                 srcInfoPoints = []}

#if 0
-- | Return the text before, within, and after a span
textTripleOfSpan :: (SrcInfo a, EndLoc a) => a -> String -> (String, String, String)
textTripleOfSpan sp s =
    let (pref, s') = splitText (getPointLoc sp) s in
    let (s'', suff) = splitText (locDiff (endLoc sp) (getPointLoc sp)) s' in
    (pref, s'', suff)
#endif

textOfSpan :: (SrcInfo a, EndLoc a) => a -> String -> String
textOfSpan sp s =
    let (_, s') = splitText (getPointLoc sp) s in
    let (s'', _) = splitText (locDiff (endLoc sp) (getPointLoc sp)) s' in
    s''

#if 0
testSpan :: (SrcInfo a, EndLoc a) => String -> a -> a
testSpan msg sp =
    case (getPointLoc sp, endLoc sp) of
      (SrcLoc _ l1 c1, SrcLoc _ l2 c2) | c1 < 1 || c2 < 1 || l1 < 1 || l2 < 1 ||
                                         l2 < l1 || (l2 == l1 && c2 < c1) -> error ("testSpan - " ++ msg)
      _ -> sp
#endif

splitText :: SrcLoc -> String -> (String, String)
splitText loc@(SrcLoc _ l0 c0) s0 =
    fst $ runState f (1, 1, "", s0)
    where
      f :: State (Int, Int, String, String) (String, String)
      f = do (l, c, r, s) <- get
             case (compare l l0, compare c c0) of
               (LT, _) ->
                   case span (/= '\n') s of
                     (r', '\n' : s') ->
                         put (l + 1, 1, r ++ r' ++ "\n", s') >> f
                     (_, "") -> case s of
                                  -- This should not happen, but if the last line
                                  -- lacks a newline terminator, haskell-src-exts
                                  -- will set the end location as if the terminator
                                  -- was present.
                                  "" -> pure (r, s)
                                  (ch : s') -> put (l, c + 1, r ++ [ch], s') >> f
                     _ -> error "splitText"
               (_, LT) ->
                   case s of
                     [] -> error ("splitText " ++ ", loc=" ++ show loc ++ ", s=" ++ show s)
                     (ch : s') -> put (l, c + 1, r ++ [ch], s') >> f
               (EQ, EQ) -> pure (r, s)
               _ -> error ("splitText - invalid arguments: loc=" ++ show loc ++ ", s=" ++ show s0)

#if 0
-- | Using n locations split a string into n + 1 segments.
-- splits (SrcLoc "" 80 20) [SrcLoc "" 80 22, SrcLoc "" 80 25, SrcLoc "" 81 4] "first line\nsecond line" ->
--   [("fi",SrcSpan ""         80 20 80 22),
--    ("rst",SrcSpan ""        80 22 80 25),
--    (" line\nsec",SrcSpan "" 80 25 81  4),
--    ("ond line",SrcSpan ""   81  4 81 12)]
splits :: SrcLoc -> [SrcLoc] -> String -> [(String, SrcSpan)]
splits offset0@(SrcLoc file _ _) locs0@(_ : _) s0 =
    zip (f offset0 locs0 s0) (map (uncurry mkSrcSpan) (zip (offset0 : locs0) (locs0 ++ [locSum offset0 (endLocOfText file s0)])))
    where
      f _ [] s = [s]
      f offset (loc : locs) s =
          let (pre, suf) = splitText (locDiff loc offset) s in
          pre : f loc locs suf
splits (SrcLoc _ _ _) [] _ = error "splits"
#endif

data Seg
    = Span (SrcLoc, SrcLoc) String
    | Between (SrcLoc, SrcLoc) String
    deriving Show

-- splits' (SrcLoc "" 80 20) [SrcSpan "" 80 20 80 22, SrcSpan "" 80 25 81 4] "first line\nsecond line"
--   [Span (SrcLoc "" 80 20), "fi", Between (SrcLoc "" 80 22) "rst",
--    Span (SrcLoc "" 80 25 81 4) " line\nsec", Between (SrcLoc "" 81 4) "ond line"]
splits' :: FilePath -> [SrcSpan] -> String -> [Seg]
splits' file spans s =
    f (SrcLoc file 1 1) spans s
    where
      f :: SrcLoc -> [SrcSpan] -> String -> [Seg]
      f offset [] s' = [Between (offset, locSum offset (endLocOfText file s')) s']
      f offset (sp : sps) s''' =
          let (pre, s') = splitText (locDiff (getPointLoc sp) offset) s''' in
          let (seg, s'') = splitText (locDiff (endLoc sp) (getPointLoc sp)) s' in
          -- trace ("offset=" ++ show offset ++ ", sp=" ++ show sp ++ ", pre=" ++ show pre ++ ", seg=" ++ show seg) $
          (if null pre then [] else [Between (offset, getPointLoc sp) pre]) ++ [Span (getPointLoc sp, endLoc sp) seg] ++ f (endLoc sp) sps s''

-- This happens, a span with end column 0, even though column
-- numbering begins at 1.  Is it a bug in haskell-src-exts?
fixSpan :: SrcSpanInfo -> SrcSpanInfo
fixSpan sp =
    if srcSpanEndColumn (srcInfoSpan sp) == 0
    then t1 $ sp {srcInfoSpan = (srcInfoSpan sp) {srcSpanEndColumn = 1}}
    else sp
    where
      t1 sp' = {-trace ("fixSpan " ++ show (srcInfoSpan sp) ++ " -> " ++ show (srcInfoSpan sp'))-} sp'

-- | Tighten the start and end points of a span to exclude any leading
-- and trailing whitespace and comments.

-- | Move the endpoint of a span to before any trailing whitespace and comments.
fixEnds :: [Comment] -> String -> SrcSpanInfo -> SrcSpanInfo
fixEnds cs s si@(SrcSpanInfo {srcInfoSpan = sp}) =
    let b@(SrcLoc _ bl bc) = realBegin si cs s in
    let e@(SrcLoc _ el ec) = realEnd si cs s in
    case (b < getPointLoc sp || b > endLoc sp || e < getPointLoc sp || e > endLoc sp) of
      True -> error "fixEnds returned position outside span"
      _ -> si {srcInfoSpan = sp {srcSpanStartLine = bl, srcSpanStartColumn = bc,
                                 srcSpanEndLine = el, srcSpanEndColumn = ec}}

-- | Given a SrcSpanInfo, find the "real" end of the object it covers,
-- meaning the position beyond which lies only whitespace and comments.
realEnd :: SrcSpanInfo -> [Comment] -> String -> SrcLoc
realEnd sp cs s =
  let b@(SrcLoc file _ _) = getPointLoc sp
      e = endLoc sp
      s'' = textOfSpan sp s
      commentSpans = map (flip spanDiff b) .
                     takeWhile ((<= e) . endLoc) .
                     dropWhile ((< b) . getPointLoc) .
                     map (\(Comment _ sp' _) -> sp') $ cs
      segs = splits' file commentSpans s'' in
  -- Use the end of the last nonspace segment
  let e' = case dropWhile isWhite (reverse segs) of
            [] -> endLocOfText file s''
            (Span (_, x) _ : _) -> x
            (Between (_, x) _ : _) -> x
      (s''', _) = splitText e' s''
      s'''' = dropWhileEnd isSpace s''' in
      locSum b (endLocOfText file s'''')
      -- e'' = locSum b e' in
  -- if r < b || r > e then error ("realEnd: sp=" ++ show sp ++ ", segs=" ++ show segs ++ " -> " ++ show e'') else e''
    where
      isWhite (Between _ s') | all isSpace s' = True
      isWhite (Span _ _) = True
      isWhite _ = False

realBegin :: SrcSpanInfo -> [Comment] -> String -> SrcLoc
realBegin sp cs s =
  let b@(SrcLoc file _ _) = getPointLoc sp
      e = endLoc sp
      s'' = textOfSpan sp s
      commentSpans = map (flip spanDiff b) .
                     takeWhile ((<= e) . endLoc) .
                     dropWhile ((< b) . getPointLoc) .
                     map (\(Comment _ sp' _) -> sp') $ cs
      segs = splits' file commentSpans s'' in
  let b' = case dropWhile isWhite segs of
            [] -> b
            (Span (x, _) _ : _) -> {-locSum b-} x
            (Between (x, _) _ : _) -> {-locSum b-} x
      (_, s''') = splitText b' s''
      b'' = endLocOfText "" (takeWhile isSpace s''') in
  foldr1 locSum [b, b', b'']
  -- if r < b || r > e then error ("realEnd: sp=" ++ show sp ++ ", segs=" ++ show segs ++ " -> " ++ show r) else r
    where
      isWhite (Between _ s') | all isSpace s' = True
      isWhite (Span _ _) = True
      isWhite _ = False

class EndLoc a where
    endLoc :: a -> SrcLoc
    srcPoints :: a -> [SrcSpan] -- a hack - we should maybe use concrete types?

instance EndLoc SrcSpan where
    endLoc x = SrcLoc (fileName x) (srcSpanEndLine x) (srcSpanEndColumn x)
    srcPoints _ = []
instance EndLoc SrcSpanInfo where
    endLoc = endLoc . srcInfoSpan
    srcPoints = srcInfoPoints
instance EndLoc a => EndLoc (Scoped a) where
    endLoc (Scoped _ x) = endLoc x
    srcPoints (Scoped _ x) = srcPoints x
instance EndLoc (SrcLoc, SrcLoc) where
    endLoc = snd
    srcPoints _ = []

#if 0
endOfDecls :: EndLoc l => Module l -> SrcLoc
endOfDecls m@(Module _l _mh _ps _ []) = endOfImports m
endOfDecls (Module _l _mh _ps _is ds) = endLoc (ann (last ds))
endOfDecls _ = error "endOfDecls"

endOfImports :: EndLoc l => Module l -> SrcLoc
endOfImports m@(Module _l _mh _ps [] _) = endOfHeader m
endOfImports (Module _l _mh _ps is _) = endLoc (ann (last is))
endOfImports _ = error "endOfImports"

endOfImportSpecs :: (EndLoc l, Show l) => ImportDecl l -> SrcLoc
endOfImportSpecs (ImportDecl {importSpecs = Just i}) =
    case srcPoints (ann i) of
      [] -> error $ "endOfImportSpecs: " ++ show i
      pts -> getPointLoc (last pts)
endOfImportSpecs (ImportDecl {importSpecs = Nothing}) = error "endOfImportSpecs"

endOfHeader :: EndLoc l => Module l -> SrcLoc
endOfHeader m@(Module _l Nothing _ps _ _) = endOfPragmas m
endOfHeader (Module _l (Just h) _ps _is _) = endLoc (ann h)
endOfHeader _ = error "endOfHeader"

endOfPragmas :: EndLoc l => Module l -> SrcLoc
endOfPragmas (Module l _ [] _ _) = endLoc l
endOfPragmas (Module _l _ ps _ _) = endLoc (ann (last ps))
endOfPragmas _ = error "endOfPragmas"
#endif

-- | Modify end locations so they precede any trailing whitespace
mapTopAnnotations :: forall a. (a -> a) -> Module a -> Module a
mapTopAnnotations fn (Module loc mh ps is ds) =
    Module loc (fmap fixMH mh) ps (map fixImport is) (map fixDecl ds)
    where
      fixMH :: ModuleHead a -> ModuleHead a
      fixMH (ModuleHead sp name warn specs) = ModuleHead (fn sp) name warn specs
      fixImport :: ImportDecl a -> ImportDecl a
      fixImport i = i {importAnn = fn (importAnn i)}
      fixDecl :: Decl a -> Decl a
      fixDecl (TypeDecl l a b) = (TypeDecl (fn l) a b)
      fixDecl (TypeFamDecl l a b c) = (TypeFamDecl (fn l) a b c)
      fixDecl (ClosedTypeFamDecl l a b c d) = (ClosedTypeFamDecl (fn l) a b c d)
      fixDecl (DataDecl l a b c d e) = (DataDecl (fn l) a b c d e)
      fixDecl (GDataDecl l a b c d e f) = GDataDecl (fn l) a b c d e f
      fixDecl (DataFamDecl l a b c) = (DataFamDecl (fn l) a b c)
      fixDecl (TypeInsDecl l a b) = (TypeInsDecl (fn l) a b)
      fixDecl (DataInsDecl l a b c d) = (DataInsDecl (fn l) a b c d)
      fixDecl (GDataInsDecl l a b c d e) = (GDataInsDecl (fn l) a b c d e)
      fixDecl (ClassDecl l a b c d) = (ClassDecl (fn l) a b c d)
      fixDecl (InstDecl l a b c) = (InstDecl (fn l) a b c)
      fixDecl (DerivDecl l a b c) = (DerivDecl (fn l) a b c)
      fixDecl (InfixDecl l a b c) = (InfixDecl (fn l) a b c)
      fixDecl (DefaultDecl l a) = (DefaultDecl (fn l) a)
      fixDecl (SpliceDecl l a) = (SpliceDecl (fn l) a)
      fixDecl (TypeSig l a b) = (TypeSig (fn l) a b)
      fixDecl (PatSynSig l a b c d e) = (PatSynSig (fn l) a b c d e)
      fixDecl (FunBind l a) = (FunBind (fn l) a)
      fixDecl (PatBind l a b c) = (PatBind (fn l) a b c)
      fixDecl (PatSyn l a b c) = (PatSyn (fn l) a b c)
      fixDecl (ForImp l a b c d e) = (ForImp (fn l) a b c d e)
      fixDecl (ForExp l a b c d) = (ForExp (fn l) a b c d)
      fixDecl (RulePragmaDecl l a) = (RulePragmaDecl (fn l) a)
      fixDecl (DeprPragmaDecl l a) = (DeprPragmaDecl (fn l) a)
      fixDecl (WarnPragmaDecl l a) = (WarnPragmaDecl (fn l) a)
      fixDecl (CompletePragma l a b) = CompletePragma (fn l) a b
      fixDecl (InlineSig l a b c) = (InlineSig (fn l) a b c)
      fixDecl (InlineConlikeSig l a b) = (InlineConlikeSig (fn l) a b)
      fixDecl (SpecSig l a b c) = (SpecSig (fn l) a b c)
      fixDecl (SpecInlineSig l a b c d) = (SpecInlineSig (fn l) a b c d)
      fixDecl (InstSig l a) = (InstSig (fn l) a)
      fixDecl (AnnPragma l a) = (AnnPragma (fn l) a)
      fixDecl (MinimalPragma l a) = (MinimalPragma (fn l) a)
      fixDecl (RoleAnnotDecl l a b) = (RoleAnnotDecl (fn l) a b)
mapTopAnnotations _ _ = error "mapTopAnnotations"