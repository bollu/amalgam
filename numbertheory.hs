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
data Prime = SymPrime String deriving(Eq, Ord)

instance Show Prime where
  show (SymPrime p) = p

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

data Number = Number [PrimePow] deriving(Eq)

-- | Empty product of primes
one :: Number
one = Number []

instance Show Number where
  show (Number []) = show 1
  show (Number ps) = mconcat (map showPrimePow ps)

-- | smart constructor to eliminate redundancies
number :: [PrimePow] -> Number
number pps = 
  let noones = [ (p, pow) |  (p, pow) <- pps, pow /= 0]
      collectpow p = sum [pow | (p', pow) <- pps, p == p']
      uniqprimes = L.nub [p | (p, _) <- noones]
  in Number [(p, collectpow p) | p <- uniqprimes]

-- infixl 5 *
-- (*) :: (Numberable a, Numberable b) => a -> b -> Number
-- (*) (toNumber -> Number ps) (toNumber -> Number qs) = number (ps <> qs)
--
numprod :: (Numberable a, Numberable b) => a -> b -> Number
numprod (toNumber -> Number ps) (toNumber -> Number qs) = number (ps <> qs)


numdiv :: (Numberable a, Numberable b) => a -> b -> Number
numdiv (toNumber -> Number xs) (toNumber -> Number ys) = 
 let primes = L.nub [p | (p, _) <- xs <> ys]
 in Number [ case (lookup p xs, lookup p ys) of 
            (Just xpow, Just ypow) ->  (p, xpow `subtract` ypow)
            (Just xpow, Nothing) ->  (p, xpow)
            (Nothing, Just ypow) ->  (p, negate ypow)
            (Nothing, Nothing) -> error "prime must be in one"
          |  p <- primes]


-- return all divisors of a number. Do this by collecting all divisors
-- of all prime powers and then take all combinations.
divisors :: Number -> [Number]
divisors (Number pps) = 
  map number (prodList (map primePowDivisors pps))

class Numberable a where
  toNumber :: a -> Number

instance Numberable Number where
  toNumber = id

instance Numberable Prime where
  toNumber p = Number [p^1]

instance Numberable PrimePow where
  toNumber p = Number [p]


type FnName = String
f :: FnName
f = "f"

-- | evaluate a FnName at a number
data Expr = 
  ExprUninterpretedFn FnName Expr 
  | ExprAdd Expr Expr
  | ExprSub Expr Expr
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

(+) :: (Exprable a, Exprable b) => a -> b -> Expr
a + b = ExprAdd (toExpr a) (toExpr b)

(-) :: (Exprable a, Exprable b) => a -> b -> Expr
a - b = ExprSub (toExpr a) (toExpr b)

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


instance Show Expr where
  show (ExprUninterpretedFn f n) = f <> "(" <> show n <> ")"
  show (ExprAdd a b) = show a <> " + " <> show b
  show (ExprSub a b) = show a <> " - " <> show b
  show (ExprMul a b) = show a <> show b
  show (ExprDiv a b) = "(" <> show a <> "/" <> show b <> ")"
  show (ExprNum n) = show n


isone :: Number -> Bool
isone (Number ps) = case number ps of Number [] -> True; _ -> False

sumExprs :: [Expr] -> Expr
sumExprs [e] = e
sumExprs (e:es) = ExprAdd e (sumExprs es)

-- | an arithmetic function
type ArithFn = Number -> Expr


-- | compute the dirichlet inverse of the given FnName applied on a number
dirchletInv :: ArithFn -> ArithFn
dirchletInv f (toExpr -> e) = undefined

-- | compute the dirichlet convolution of a function against another
dirchletConv :: ArithFn -> ArithFn -> ArithFn
dirchletConv f g n = sumExprs [  (f  (n `numdiv` d)) * (g  d) | d <- divisors n]

main :: IO ()
main = print ""
