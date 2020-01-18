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
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Ratio

infixl 0 |>
(|>) :: x -> (x -> y) -> y
(|>) x f = f x 

type FnName = String

-- | Rational number
type QQ = Ratio Int

-- | factors contain symbolic primes, regular numbers, and uninterpreted functions.
data Factor = SymPrime String | SymFn FnName Term deriving(Eq, Ord)
-- | terms are products of factors of different powers
data Term = Term { termCoeff :: QQ, termFactors :: M.Map Factor Int } deriving(Eq, Ord)

-- | take the product of two terms.
multerm_ :: Term -> Term -> Term
multerm_ (Term q1 t1) (Term q2 t2) =
  (M.mergeWithKey 
    (\f a b -> if a + b == 0 then Nothing else Just (a + b)) -- both
    id id t1 t2) |> term_ (q1 * q2)

-- | reciprocal
recipterm :: Term -> Term 
recipterm (Term q t) = M.map negate t |> Term (recip q)

divterm_ :: Term -> Term -> Term
divterm_ a b = a `multerm_` recipterm b

instance Num Term where
  fromInteger i = Term (fromInteger i) M.empty
  (*) = multerm_

instance Show Factor where
  show (SymPrime n) = n
  show (SymFn n x) = n <> show x 

instance Show Term where
  show (Term q t) = 
    let ns = M.filter (>= 0) t
        ds = M.filter (< 0) t
        -- | show a power of a factor. Normalize powers since we are showing
        -- denominator using / (...)
        showfac f (abs -> 1) = show f
        showfac f n = show f <> "^" <> show (abs n)
        -- | numerator & denominator 
        nstr = (if numerator q /= 1 || (M.null ns) then show (numerator q) else "") <> 
                M.foldMapWithKey showfac ns
        dstr = (if denominator q /= 1 then show (denominator q) else "") <> 
                M.foldMapWithKey showfac ds
    in  "(" <>  nstr <> (if not (null dstr) then "/" <> dstr <> ")" else ")")

-- | construct and normalize a term
term_ :: QQ -> M.Map Factor Int -> Term
term_ q t = 
  let nopowzero = M.filter (/= 0) t
  in if q == 0 then zero else  Term q nopowzero

term :: Termable a => a -> Term
term (toTerm -> t) = case t of Term q f2pow -> term_ q f2pow


number2term :: Int -> Term
number2term i = Term (i % 1) M.empty

one :: Term
one = number2term 1

zero :: Term
zero = number2term 0

minusone :: Term
minusone = number2term (-1)

-- | Return if the term is a constant number
term2number_ :: Term -> Maybe QQ
term2number_ (Term q t) = if M.null t then Just q else Nothing

iszero_ :: Term -> Bool
iszero_ t = term2number_ t == Just 0

iszero :: Termable a => a -> Bool
iszero a = a |> toTerm |> iszero_


-- | return all possible divisors of a term.
divisors_ :: Term -> S.Set Term
divisors_ (Term q f2pow) = 
  f2pow |> M.map (\pow -> [0..pow]) |> sequenceA |> map (term_ q) |> S.fromList

divisors :: Exprable a => a -> S.Set Term
divisors (toExpr -> e) = toTerm e |> divisors_

class Termable a where
  toTerm :: a -> Term

instance Termable Factor where
  toTerm f = M.singleton f 1 |> Term 1

instance Termable Term where
  toTerm t = t

instance Termable Expr where
  toTerm (Expr [t]) = t
  toTerm e = 
    error ("unable to convert expression to term: |" <> show e <> "|")


-- | Expressions are expressions involving terms
data Expr = Expr [Term]

instance Show Expr where
 show (Expr ts) = 
   -- | smaller coefficients first
   let ts' = L.sortBy (\t t' -> (termCoeff t' |> abs) `compare` (termCoeff t |> abs)) ts
   in L.intercalate "+" (map show ts')

expradd :: Expr -> Expr -> Expr
expradd (Expr e) (Expr e') = Expr (e <> e')

instance Num Expr where
  fromInteger i = fromInteger i |> number2term |> toExpr
  (+) = expradd

class Exprable a where
  toExpr :: a -> Expr

instance Exprable Expr where
  toExpr = id

instance Exprable Term where
  toExpr t = Expr [t]

instance Exprable Factor where
  toExpr f = Expr [toTerm f]

-- | create equivalence classes
equiv :: (a -> a -> Bool) -> [a] -> [[a]]
equiv _ [] = []
equiv f (x:as) = let (xs, ys) = L.partition (f x) (x:as) in xs:equiv f ys

normalizeExpr :: Expr -> Expr
normalizeExpr (Expr ts) = 
 let equivClasses = equiv (\t t' -> termFactors t == termFactors t') ts
     sumcoeffs ts = getSum (foldMap (Sum . termCoeff) ts)
     classfactors ts = termFactors (ts !! 0) -- ^ get representative
 in Expr [term_ (sumcoeffs ts) (classfactors ts)| ts <- equivClasses]


-- arithmetic function: takes terms and returns terms
type ArithFn = Term -> Term

narith :: ArithFn
narith = id

-- | we need floor to define this
idarith :: ArithFn
idarith t = if term2number_ t == Just 1 then toTerm one else toTerm zero

f,g :: ArithFn
f t = SymFn "f" t |> term
g t = SymFn "g" t |> term

p, q, r, s :: Term
p = SymPrime "p" |> term
q = SymPrime "q" |> term
r = SymPrime "r" |> term
s = SymPrime "s" |> term


-- | compute the symbolic dirichlet inverse of an arithmetic function
dinv :: ArithFn -> Term -> Expr
dinv f 1 = recipterm (f 1) |> toExpr
dinv f n = sum [((number2term (-1)) `divterm_` (f (n `divterm_` d)) |> toExpr) * (dinv f d) | d <- S.toList (divisors n), d /= n]


dinv_ :: (Term -> Term) -> Term -> [Term]
dinv_ f 1 = [recipterm (f 1)]
dinv_ f n = do
  d <- S.toList (divisors n)
  guard (d /= n)
  invd <- dinv_ f d
  ((minusone `divterm_` (f 1)) * (f (n `divterm_` d))) * invd |> return

main :: IO ()
main = do
  print "dirichlet inverse of f at 1"
  print (dinv_ f 1)

  print "dirichlet inverse of f at p"
  print (dinv_ f p)

  print "dirichlet inverse of f at pq"
  print (dinv_ f (p*q))
  print (dinv_ f (p*q) |> Expr |> normalizeExpr)

  print "dirichlet inverse of f at pqr"
  print (dinv_ f (p*q*r))
  print (dinv_ f (p*q*r) |> Expr |> normalizeExpr)

  print "dirichlet inverse of f at pqrs"
  print (dinv_ f (p*q*r*s) |> Expr |> normalizeExpr)
