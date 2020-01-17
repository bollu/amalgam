{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
import Prelude hiding ((^), (^^), (+), (-), (*), ($))
import qualified Data.Map as M
import qualified Data.List as L
-- symbolic prime
type Prime = String
p, q, r :: Prime
p = "p"
q = "q"
r = "r"

data PrimePow = PrimePow Prime Int

instance Show PrimePow where
  show (PrimePow p 1) = p
  show (PrimePow p 0) = "1"
  show (PrimePow p n) = p <> "^" <> show n

(^) :: Prime -> Int -> PrimePow
(^) = PrimePow


-- return all divisors of a prime power
primePowDivisors :: PrimePow -> [PrimePow]
primePowDivisors (PrimePow p n) = [p ^ i | i <- [0..n]]

-- [(p, 1), (q, 2)]
-- [[(p, 0), (p, 1)], [(q, 0), (q, 1)]
-- [[a]] -> [[a]]
prodList :: [[a]] -> [[a]]
prodList [as] = map (\a -> [a]) as
prodList (as:aas) = do
  as' <- prodList aas
  a <- as
  return (a:as')

data Number = Number [PrimePow]

-- | smart constructor to eliminate redundancies
number :: [PrimePow] -> Number
number pps = 
  let noones = [ (p, pow) | PrimePow p pow <- pps, pow /= 0]
      collectpow p = sum [pow | PrimePow p' pow <- pps, p == p']
      uniqprimes = L.nub [p | (p, _) <- noones]
  in Number [PrimePow p (collectpow p) | p <- uniqprimes]

infixl 5 *
(*) :: (Numberable a, Numberable b) => a -> b -> Number
(*) (toNumber -> Number ps) (toNumber -> Number qs) = number (ps <> qs)


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
  toNumber p = Number ([PrimePow p 1])

instance Numberable PrimePow where
  toNumber p = Number [p]

-- empty product of primes
one :: Number
one = Number []


instance Show Number where
  show (Number []) = show 1
  show (Number ps) = mconcat (map show ps)

type Function = String
f :: Function
f = "f"

-- | evaluate a function at a number
data Expr = ExprEval Function Number | ExprAdd Expr Expr | ExprSub Expr Expr | ExprNum Number

class Exprable a where
  toExpr :: a -> Expr

instance Numberable a => Exprable a where
  toExpr = ExprNum . toNumber

instance Exprable Expr where
  toExpr = id
  


($) :: Function -> Number -> Expr
($) = ExprEval

(+) :: (Exprable a, Exprable b) => a -> b -> Expr
a + b = ExprAdd (toExpr a) (toExpr b)

(-) :: Expr -> Expr -> Expr
(-) = ExprSub

instance Show Expr where
  show (ExprEval f n) = f <> "(" <> show n <> ")"
  show (ExprAdd a b) = show a <> " + " <> show b
  show (ExprSub a b) = show a <> " - " <> show b
  show (ExprNum n) = show n

main :: IO ()
main = print ""
