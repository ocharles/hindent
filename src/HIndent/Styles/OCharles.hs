{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ocharles' style.

module HIndent.Styles.OCharles
  (ocharles)
  where

import Prelude hiding (exp, sequence_)

import Control.Applicative
import Control.Monad (replicateM_)
import Control.Monad.State (get)
import Data.Foldable (for_, sequence_, traverse_)
import Data.List (intersperse)
import Data.Ord (comparing)

import qualified HIndent.Styles.ChrisDone as ChrisDone
import qualified HIndent.Pretty as HIndent
import qualified HIndent.Types as HIndent
import qualified Language.Haskell.Exts.Annotated.Syntax as HS

data State = State

--------------------------------------------------------------------------------
ocharles :: HIndent.Style
ocharles =
  HIndent.Style
    { HIndent.styleName =
        "ocharles"
    , HIndent.styleAuthor =
        "Oliver Charles"
    , HIndent.styleDescription =
        "ocharles's personal style"
    , HIndent.styleInitialState =
        State
    , HIndent.styleExtenders =
        [HIndent.Extender exp
        ,HIndent.Extender match
        ,HIndent.Extender rhs
        ,HIndent.Extender alt
        ,HIndent.Extender decl
        ,HIndent.Extender ty
        ,HIndent.Extender stmt]
    , HIndent.styleDefConfig =
        HIndent.Config
          { HIndent.configMaxColumns =
              80
          , HIndent.configIndentSpaces =
              2
          }
    }

--------------------------------------------------------------------------------
rhs :: s -> HS.Rhs HIndent.NodeInfo -> HIndent.Printer ()
rhs _ (HS.UnGuardedRhs _ e) = do
  case e of
    HS.Do{} -> do
      HIndent.write " = "
      HIndent.pretty e

    _ -> do
      indentSpaces <- HIndent.getIndentSpaces
      HIndent.indented
        indentSpaces
        (ChrisDone.dependOrNewline (HIndent.write " = ")
                                   e
                                   HIndent.pretty)

rhs _ e = HIndent.prettyNoExt e


--------------------------------------------------------------------------------
exp :: s -> HS.Exp HIndent.NodeInfo -> HIndent.Printer ()
exp s app@(HS.App{}) = ChrisDone.exp s app

exp _ (HS.Lambda _ pats e) = do
  HIndent.write "\\"
  HIndent.spaced $
    map HIndent.pretty pats
  HIndent.write " -> "
  inlineOrSwing (HIndent.pretty e)

exp _ (HS.InfixApp _ l op@(HS.QVarOp _ (HS.UnQual _ (HS.Symbol _ "$"))) r)
  | flattensDollar r =
    do
      HIndent.pretty l
      HIndent.space
      HIndent.pretty op
      HIndent.space
      HIndent.pretty r

exp s app@(HS.InfixApp{}) = ChrisDone.exp s app

exp _ (HS.Case _ e alts) = do
  HIndent.depend
    (HIndent.write "case" >>
     HIndent.space)
    (HIndent.pretty e >> HIndent.space >>
     HIndent.write "of")
  HIndent.newline
  HIndent.indented 2 $ do
    curS <- get
    states <- mapM (fmap snd . HIndent.sandbox . printAltInline) alts
    if all (fitsOnOneLine curS) states
       then sequence_ (intersperse HIndent.newline (map printAltInline alts))
       else sequence_ (intersperse (replicateM_ 2 HIndent.newline)
                                   (map HIndent.pretty alts))

  where
  fitsOnOneLine s s' =
    let multiLines = comparing HIndent.psLine s' s == GT
        overflows = HIndent.psColumn s' > HIndent.configMaxColumns (HIndent.psConfig s)
    in not multiLines && not overflows

  printAltInline (HS.Alt _ pat (HS.UnGuardedAlt _ e) Nothing) = do
    HIndent.pretty pat
    HIndent.space
    HIndent.write "->"
    HIndent.space
    HIndent.indented 2 (HIndent.pretty e)

  printALtInline alt = HIndent.pretty alt

exp _ (HS.Do _ stmts) = do
  HIndent.write "do"
  HIndent.newline
  HIndent.indented
    2
    (HIndent.lined $
     map HIndent.pretty stmts)

exp _ (HS.RecConstr _ name fields) = do
  HIndent.pretty name
  HIndent.newline
  HIndent.indented 2 $
    do
      HIndent.string "{"
      case fields of
        [f] ->
          HIndent.indented 2 $ do
            HIndent.space
            HIndent.pretty f
            HIndent.space

        [] ->
          return ()

        (f:fs) -> do
          HIndent.space
          HIndent.indented 2
                           (HIndent.pretty f)
          HIndent.newline
          for_ fs $
            \f' ->
              do
                HIndent.comma
                HIndent.space
                HIndent.indented 2 $
                  HIndent.pretty f'
                HIndent.newline
      HIndent.string "}"

exp _ e = HIndent.prettyNoExt e

--------------------------------------------------------------------------------
stmt :: s -> HS.Stmt HIndent.NodeInfo -> HIndent.Printer ()
stmt _ (HS.Generator _ pat e) = do
  HIndent.pretty pat
  HIndent.space
  HIndent.write "<-"
  inlineOrSwing (HIndent.pretty e)

stmt s x = ChrisDone.stmt s x


--------------------------------------------------------------------------------
inlineOrSwing :: HIndent.Printer a -> HIndent.Printer a
inlineOrSwing p = do
  lim <- HIndent.getColumnLimit
  (_,s) <- HIndent.sandbox p
  curS <- get
  let multiLines = comparing HIndent.psLine s curS == GT
      overflows = HIndent.psColumn s > lim
  if multiLines || overflows
     then do
            HIndent.newline
            HIndent.indented 2 p
     else HIndent.space >> p


--------------------------------------------------------------------------------
flattensDollar HS.Lambda{} = True
flattensDollar HS.Do{} = True
flattensDollar _ = False


--------------------------------------------------------------------------------
-- | These are expressions that we would ideally render on one line
isFlat HS.Lit{} = True
isFlat HS.Var{} = True
isFlat HS.App{} = True
isFlat HS.InfixApp{} = True
isFlat HS.List{} = True
isFlat _ = False


--------------------------------------------------------------------------------
alt :: s -> HS.Alt HIndent.NodeInfo -> HIndent.Printer ()
alt _ (HS.Alt _ pat (HS.UnGuardedAlt _ do'@(HS.Do{})) binds) = do
  HIndent.pretty pat
  HIndent.space
  HIndent.write "->"
  HIndent.space
  HIndent.pretty do'

alt _ a = HIndent.prettyNoExt a


--------------------------------------------------------------------------------
match :: s -> HS.Match HIndent.NodeInfo -> HIndent.Printer ()
match _ (HS.Match _ name pats r mbinds) = do
  HIndent.depend (HIndent.pretty name >> HIndent.space)
                 (HIndent.spaced (map HIndent.pretty pats))
  HIndent.pretty r
  for_ mbinds $ \binds -> do
    HIndent.newline
    indentSpaces <- HIndent.getIndentSpaces
    HIndent.indented indentSpaces $ do
      HIndent.write "where"
      HIndent.newline
      HIndent.pretty binds

match _ x = HIndent.prettyNoExt x


--------------------------------------------------------------------------------
decl :: s -> HS.Decl HIndent.NodeInfo -> HIndent.Printer ()
decl _ (HS.DataDecl _ don ctx h@(HS.DHead{}) [(HS.QualConDecl _ _ _ (HS.RecDecl _ n (f1:fields@(_:_))))] deriv) = do
  HIndent.pretty don
  HIndent.space
  traverse_ HIndent.pretty ctx
  HIndent.pretty h
  HIndent.space
  HIndent.write "="
  HIndent.space
  HIndent.pretty n
  HIndent.newline
  HIndent.indented 2 $ do
    HIndent.depend (HIndent.write "{" >> HIndent.space)
                   (HIndent.pretty f1)
    HIndent.newline
    for_ fields $ \f -> do
      HIndent.depend (HIndent.write "," >> HIndent.space)
                     (HIndent.pretty f)
      HIndent.newline
    HIndent.write "}"
    for_ deriv $ \(HS.Deriving _ ds) -> do
      HIndent.newline
      HIndent.write "deriving"
      HIndent.space
      HIndent.parens $ HIndent.commas $ map HIndent.pretty ds

decl _ x = HIndent.prettyNoExt x


--------------------------------------------------------------------------------
ty :: s -> HS.Type HIndent.NodeInfo -> HIndent.Printer ()
ty _ (HS.TyTuple _ boxed tys) = do
  HIndent.depend (HIndent.write (case boxed of HS.Unboxed -> "(#"
                                               HS.Boxed -> "("))
         (do sequence_ $ intersperse (HIndent.write "," >> HIndent.space) $ map HIndent.pretty tys
             HIndent.write (case boxed of HS.Unboxed -> "#)"
                                          HS.Boxed -> ")"))
ty _ x = HIndent.prettyNoExt x
