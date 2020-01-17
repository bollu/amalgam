{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE Rank2Types #-}
import Prelude hiding ((^), (^^), (+), (-), (*), (/), ($))
import qualified Data.Map as M
import qualified Data.List as L

-- symbolic prime
data Prime = SymPrime String | Zero deriving(Eq, Ord)


instance Show Prime where
  show (SymPrime p) = p
  show Zero = "0"

p, q, r :: Prime
p = SymPrime "p"
q = SymPrime "q"
r = SymPrime "r"

type PrimePow = (Prime, Int)

showPrimePow :: PrimePow -> String
showPrimePow (p, 0) = show 1
showPrimePow (p, 1) = show p
showPrimePow (p, n) = show p <> "^" <> show n

(^) :: Prime -> Int -> PrimePow
(^) = (,)

-- return all divisors of a prime power
primePowDivisors :: PrimePow -> [PrimePow]
primePowDivisors (p, n) = [p ^ i | i <- [0..n]]

-- [(p, 1), (q, 2)]
-- [[(p, 0), (p, 1)], [(q, 0), (q, 1)]
-- [[a]] -> [[a]]
prodList :: [[a]] -> [[a]]
prodList [as] = map (\a -> [a]) as
prodList (as:aas) = do
  as' <- prodList aas
  a <- as
  return (a:as')

data Sign = SignPositive | SignNegative deriving(Eq)

instance Semigroup Sign where
  SignNegative <> SignNegative = SignPositive
  SignPositive <> s = s
  s <> SignPositive = s

instance Monoid Sign where
 mempty = SignPositive

instance Show Sign where
  show (SignPositive) = "+"
  show (SignNegative) = "-"

data Number = Number Sign [PrimePow] deriving(Eq)

-- | Empty product of primes
one :: Number
one = Number SignPositive []

minusone :: Number
minusone = Number SignNegative []

zero :: Number
zero = Number SignPositive [(Zero, 1)]

instance Show Number where
  show (Number SignPositive []) = show 1
  show (Number SignNegative []) = show (-1)
  show (Number SignPositive ps) = mconcat (map showPrimePow ps)
  show (Number SignNegative ps) = "-" <> mconcat (map showPrimePow ps)

-- | smart constructor to eliminate redundancies
number :: Number -> Number
number (Number s pps) = 
  let noones = [ (p, pow) |  (p, pow) <- pps, pow /= 0]
      collectpow p = sum [pow | (p', pow) <- pps, p == p']
      uniqprimes = L.nub [p | (p, _) <- noones]
      haszero = any (\(p, _) -> p == Zero) pps
  in if haszero
      then zero
      else Number s [(p, collectpow p) | p <- uniqprimes]

-- infixl 5 *
-- (*) :: (Numberable a, Numberable b) => a -> b -> Number
-- (*) (toNumber -> Number ps) (toNumber -> Number qs) = number (ps <> qs)
--
numprod :: (Numberable a, Numberable b) => a -> b -> Number
numprod (toNumber -> Number s1 ps) (toNumber -> Number s2 qs) = 
  number (Number (s1 <> s2) (ps <> qs))


numdiv :: (Numberable a, Numberable b) => a -> b -> Number
numdiv (toNumber -> Number s1 xs) (toNumber -> Number s2 ys) = 
 let primes = L.nub [p | (p, _) <- xs <> ys]
 in Number (s1 <> s2) [ case (lookup p xs, lookup p ys) of 
                                (Just xpow, Just ypow) ->  (p, xpow `subtract` ypow)
                                (Just xpow, Nothing) ->  (p, xpow)
                                (Nothing, Just ypow) ->  (p, negate ypow)
                                (Nothing, Nothing) -> error "prime must be in one"
                      | p <- primes]
   

-- return all divisors of a number. Do this by collecting all divisors
-- of all prime powers and then take all combinations.
divisors :: Number -> [Number]
divisors (Number s pps) = 
  map (\ps -> number (Number s ps)) (prodList (map primePowDivisors pps))

class Numberable a where
  toNumber :: a -> Number

instance Numberable Number where
  toNumber = id

instance Numberable Prime where
  toNumber p = Number SignPositive [p^1]

instance Numberable PrimePow where
  toNumber p = Number SignPositive [p]


type FnName = String

-- | evaluate a FnName at a number
data Expr = 
  ExprUninterpretedFn FnName Expr 
  | ExprAdd Expr Expr
  | ExprDiv Expr Expr
  | ExprMul Expr Expr
  | ExprNum Number
  deriving(Eq)

class Exprable a where
  toExpr :: a -> Expr

{-# OVERLAPPLING #-}
instance Numberable a => Exprable a where
  toExpr = ExprNum . toNumber

{-# OVERLAPPED #-}
instance Exprable Expr where
  toExpr = id

($) :: Exprable  a => FnName -> a -> Expr
f $ a = ExprUninterpretedFn f (toExpr a)

-- | Convention: keep numbers to the left
(+) :: (Exprable a, Exprable b) => a -> b -> Expr
a + b = case (toExpr a, toExpr b) of
             (ExprNum na, b) -> if iszero na then b else ExprAdd (ExprNum na) b
             (a, ExprNum nb) -> if iszero nb then a else ExprAdd (ExprNum nb) a
             (a, b) -> ExprAdd a b

(-) :: (Exprable a, Exprable b) => a -> b -> Expr
a - b = ExprAdd (toExpr a) (minusone * (toExpr b))

(*) :: (Exprable a, Exprable b) => a -> b -> Expr
a * b = case (toExpr a, toExpr b) of
          (ExprNum a, ExprNum b) -> ExprNum (numprod a b)
          (ExprNum a, b) -> ExprMul (ExprNum a) b
          (a, ExprNum b) -> ExprMul (ExprNum b) a
          (a, b) -> ExprMul a b

(/) :: (Exprable a, Exprable b) => a -> b -> Expr
a / b = case (toExpr a, toExpr b) of
          (ExprNum a, ExprNum b) -> ExprNum (numdiv a b)
          (ExprNum a, b) -> ExprDiv (ExprNum a) b
          (a, ExprNum b) -> ExprDiv (ExprNum b) a
          (a, b) -> ExprDiv a b

-- pull division out
ne1 :: Expr -> Expr
ne1 (ExprMul (ExprDiv a b) (ExprDiv x y)) = ExprDiv (ExprMul a b) (ExprMul x y)
ne1 (ExprMul a (ExprDiv b c)) = ExprDiv (ExprMul a b) c
ne1 (ExprMul (ExprDiv a b) c) = ExprDiv (ExprMul a c) b
ne1 (ExprMul a b) = ExprMul (ne1 a) (ne1 b)
ne1 (ExprDiv a b) = ExprDiv (ne1 a) (ne1 b)
ne1 (ExprAdd a b) = ExprAdd (ne1 a) (ne1 b)
ne1 x = x

normalizeExpr :: Expr -> Expr
normalizeExpr e = 
  let e1 = ne1 e
  in if e1 == e then e else normalizeExpr e1


instance Show Expr where
  show (ExprUninterpretedFn f n) = f <> "(" <> show n <> ")"
  show (ExprAdd a b) = show a <> " + " <> show b
  show (ExprMul a b) = show a <> show b
  show (ExprDiv a b) = "(" <> show a <> "/" <> show b <> ")"
  show (ExprNum n) = show n


isone :: Number -> Bool
isone n = case n of Number _ [] -> True; _ -> False

iszero :: Number -> Bool
iszero n = case n of Number _ [(Zero, 1)] -> True; _ -> False

sumExprs :: [Expr] -> Expr
sumExprs [e] = e
sumExprs (e:es) = e + (sumExprs es)

prodExprs :: [Expr] -> Expr
prodExprs [e] = e
prodExprs (e:es) = e * (sumExprs es)

-- | an arithmetic function
type ArithFn = Number -> Expr

f :: ArithFn
f n = "f" $  n

-- | compute the dirichlet inverse of the given FnName applied on a number
dirchletInv :: ArithFn -> ArithFn
dirchletInv f n = 
  if isone n
  then one / (f  one)
  else sumExprs [(minusone / (f one)) * f (n `numdiv` d) * (dirchletInv f d)| d <- divisors n, d /= n]

-- I (i) = 1 if i == 1, 0 otherwise
identity :: ArithFn
identity n = if isone n then ExprNum one else ExprNum zero

-- n(i) = i
n :: ArithFn
n i = ExprNum i

-- | compute the dirichlet convolution of a function against another
dirchletConv :: ArithFn -> ArithFn -> ArithFn
dirchletConv f g n = sumExprs [  (f  (n `numdiv` d)) * (g  d) | d <- divisors n]

main :: IO ()
main = print ""
