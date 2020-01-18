{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BlockArguments #-}
import Prelude hiding ((^^), (/), ($))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid

infixl 0 |>
(|>) :: x -> (x -> y) -> y
(|>) x f = f x 

type FnName = String

-- | factors contain symbolic primes, regular numbers, and uninterpreted functions.
data Factor = Number Int | SymPrime String | SymFn FnName Term deriving(Eq, Ord)
-- | terms are products of factors of different powers
data Term = Term (M.Map Factor Int) deriving(Eq, Ord)
-- | Expressions are expressions involving  terms
data Expr = Expr [Term] deriving (Eq, Ord)

-- | take the product of two terms.
multerm_ :: Term -> Term -> Term
multerm_ (Term t1) (Term t2) =
  (M.mergeWithKey 
    (\f a b -> if a + b == 0 then Nothing else Just (a + b)) -- both
    id id t1 t2) |> term_

instance Num Term where
  fromInteger i = fromInteger i |> Number |> toTerm
  (*) = multerm_

instance Show Factor where
  show (SymPrime n) = n
  show (Number i ) = show i
  show (SymFn n x) = n <> "(" <> show x <> ")"

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
      nopowzero = M.filter (/= 0) nonumbers
      (Product numprod) = M.foldMapWithKey (\(Number n) pow -> Product (n ^ pow)) numbers
  in nopowzero |> M.insert (Number numprod) 1 |> Term

term :: Termable a => a -> Term
term (toTerm -> t) = case t of Term f2pow -> term_ f2pow


-- | Return if the term is a constannt number
termnumber_ :: Term -> Maybe Int
termnumber_ (Term t) = 
  let (Term t') = term_ t 
      nums = M.filterWithKey (\f _ -> case f of Number _ -> True; _ -> False) t'
      nonums = M.filterWithKey (\f _ -> case f of Number _ -> False; _ -> True) t
  in case M.null nonums of
       False -> Nothing
       True -> case M.keys nums of
                 [] -> Just 1
                 [Number n] -> Just n
                 xs -> error "normalized term cannot have more than one number"

iszero_ :: Term -> Bool
iszero_ t = termnumber_ t == Just 0

iszero :: Termable a => a -> Bool
iszero a = a |> toTerm |> iszero_


-- | return all possible divisors of a term.
divisors_ :: Term -> S.Set Term
divisors_ (Term f2pow) = 
  f2pow |> M.map (\pow -> [0..pow]) |> sequenceA |> map term_ |> S.fromList

divisors :: Exprable a => a -> S.Set Term
divisors (toExpr -> e) = toTerm e |> divisors_

one :: Factor
one = Number 1

zero :: Factor
zero = Number 0

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

expradd :: Expr -> Expr -> Expr
expradd (Expr e) (Expr e') = Expr (e <> e')

exprsub :: Expr -> Expr -> Expr
exprsub (Expr e) (Expr e') = Expr (e <> (map (* (-1)) e'))

exprmul :: Expr -> Expr -> Expr
exprmul (Expr e) (Expr e') = liftA2 multerm_ e e' |> Expr

instance Num Expr where
  fromInteger i = fromInteger i |> Number |> toExpr
  (+) = expradd
  (-) = exprsub
  (*) = exprmul

-- arithmetic function: takes terms and returns terms
type ArithFn = Term -> Term

narith :: ArithFn
narith = id

-- | we need floor to define this
idarith :: ArithFn
idarith t = if termnumber_ t == Just 1 then toTerm one else toTerm zero

f,g :: ArithFn
f t = SymFn "f" t |> term
g t = SymFn "g" t |> term

p, q, r :: Term
p = SymPrime "p" |> term
q = SymPrime "q" |> term
r = SymPrime "r" |> term

main :: IO ()
main = do
  print "dirichlet convolution of pq"
