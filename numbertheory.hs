{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE Rank2Types #-}
import Prelude hiding ((^^), (+), (/), ($))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Foldable
import Data.Traversable
import Data.Monoid

-- | this is stupid, but GHC.Num for some reason exposes sub but not add?
add :: Num a => a -> a -> a
add a b = a `subtract` (negate b)

infixl 0 |>
(|>) :: x -> (x -> y) -> y
(|>) x f = f x 

type FnName = String

-- | factors contain symbolic primes, regular numbers, and uninterpreted functions.
data Factor = Number Int | SymPrime String | SymFn FnName Expr deriving(Eq, Ord)
-- | terms are products of factors of different powers
data Term = Term (M.Map Factor Int) deriving(Eq, Ord)
-- | Expressions are sums of terms
data Expr = Expr [Term] deriving (Eq, Ord)

-- | take the product of two terms.
multerm :: Term -> Term -> Term
multerm (Term t1) (Term t2) =
  (M.mergeWithKey 
    (\f a b -> if a `add` b == 0 then Nothing else Just (a `add` b)) -- both
    id id t1 t2) |> Term

instance Show Factor where
  show (SymPrime n) = n
  show (Number i ) = show i
  show (SymFn n e) = n <> "(" <> show e <> ")"

instance Show Term where
  show (Term t) = 
    let ns = M.filter (>= 0) t
        ds = M.filter (< 0) t
        -- | show a power of a factor. Normalize powers since we are showing
        -- denominator using / (...)
        showfac f (abs -> 1) = show f
        showfac f n = show f <> "^" <> show (abs n)
        -- | numerator & denominator 
        shown = if M.null ns then "1" else (M.foldMapWithKey showfac ns)
        showd = if M.null ds then [] else "/" <> (M.foldMapWithKey showfac ds) 
    in  "("  <>  shown <> showd <> ")"

instance Show Expr where
 show (Expr ts) = L.intercalate "+" (map show ts)

-- | construct and normalize a term
term_ :: M.Map Factor Int -> Term
term_ t = 
  let numbers = M.filterWithKey (\f _ -> case f of Number _ -> True; _ -> False) t
      nonumbers = M.filterWithKey (\f _ -> case f of Number _ -> False; _ -> True) t
      (Product numprod) = M.foldMapWithKey (\(Number n) pow -> Product (n ^ pow)) numbers
  in nonumbers |> M.insert (Number numprod) 1 |> Term


-- | return all possible divisors of a term.
divisors_ :: Term -> S.Set Term
divisors_ (Term f2pow) = 
  f2pow |> M.map (\pow -> [0..pow]) |> sequenceA |> map term_ |> S.fromList

divisors :: Exprable a => a -> S.Set Term
divisors (toExpr -> e) = toTerm e |> divisors_

one_ :: Factor
one_ = Number 1

one :: Expr
one = toExpr one_

class Termable a where
  toTerm :: a -> Term

instance Termable Factor where
  toTerm f = M.singleton f 1 |> Term

instance Termable Term where
  toTerm t = t

instance Termable Expr where
  toTerm (Expr [t]) = t
  toTerm e = 
    error ("unable to convert expression to term: |" <> show e <> "|")

class Exprable a where
  toExpr :: a -> Expr

instance Exprable Expr where
  toExpr = id

instance Exprable Term where
  toExpr t = Expr [t]

instance Exprable Factor where
  toExpr f = Expr [toTerm f]

