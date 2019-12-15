-- import List
import Numeric
import Control.Monad
-- import System
import Text.XHtml.Strict
import Data.Unique

data Currency = USD | GBP | EUR | ZAR | KYD | CHF  deriving (Eq, Show)
type Date = (CalendarTime, TimeStep)
type TimeStep = Int
type CalendarTime = ()

mkDate :: TimeStep -> Date
mkDate s = ((),s)

time0 :: Date
time0 = mkDate 0

newtype Obs a = Obs (Date -> PR a)
instance Show a => Show (Obs a) where
  show (Obs o) = let (PR (rv:_)) = o time0 in "(Obs " ++ show rv ++ ")"


data Contract =
    Zero
  | One  Currency
  | Give Contract
  | And  Contract Contract
  | Or   Contract Contract
  | Cond    (Obs Bool)   Contract Contract
  | Scale   (Obs Double) Contract
  | When    (Obs Bool)   Contract
  | Anytime (Obs Bool)   Contract
  | Until   (Obs Bool)   Contract
  deriving Show

one :: Currency -> Contract
one = One

give :: Contract -> Contract
give = Give

cAnd :: Contract -> Contract -> Contract
cAnd = And

cOr :: Contract -> Contract -> Contract
cOr = Or

cond :: Obs Bool -> Contract -> Contract -> Contract
cond = Cond

scale :: Obs Double -> Contract -> Contract
scale = Scale

cWhen :: Obs Bool -> Contract -> Contract
cWhen = When

anytime :: Obs Bool -> Contract -> Contract
anytime = Anytime

cUntil :: Obs Bool -> Contract -> Contract
cUntil = Until

andGive :: Contract -> Contract -> Contract
andGive c d = c `cAnd` give d

zcb :: Date -> Double -> Currency -> Contract
zcb t x k = cWhen (at t) (scale (konst x) (one k))

t1 :: Date
t1 = mkDate t1Horizon

t1Horizon = 3 :: TimeStep

c1 :: Contract
c1 = zcb t1 10 USD