module Main (main) where

import Test.HUnit

import Prelude
import Control.Lens ((#))
import Control.Monad (when)
import System.Exit (exitFailure)

import Data.Radian

t180 :: Assertion
t180 = toRadians # (180 :: Double) @?= pi

t90 :: Assertion
t90 = toRadians # (90 :: Double) @?= 1.5707963267948966

t359 :: Assertion
t359 = toRadians # (359 :: Double) @?= 6.265732014659643

t360 :: Assertion
t360 = toRadians # (360 :: Double) @?= 6.283185307179586

t3600 :: Assertion
t3600 = toRadians # (3600 :: Double) @?= 62.83185307179586

t1 :: Assertion
t1 = toRadians # (1 :: Double) @?= 1.7453292519943295e-2

tneg180 :: Assertion
tneg180 = toRadians # ((-180) :: Double) @?= -pi

f0 :: Assertion
f0 = fromRadians # (0 :: Double) @?= 0.0

f1 :: Assertion
f1 = fromRadians # (1 :: Double) @?= 57.29577951308232

fneg1 :: Assertion
fneg1 = fromRadians # ((-1) :: Double) @?= -57.29577951308232

f3 :: Assertion
f3 = fromRadians # (3 :: Double) @?= 171.88733853924697

tests :: Test
tests =
  TestList $ fmap TestCase [
    t180
  , t90
  , t359
  , t360
  , t3600
  , t1
  , tneg180
  , f0
  , f1
  , fneg1
  , f3
  ]

main :: IO ()
main = do
  c <- runTestTT tests
  when (errors c > 0 || failures c > 0) exitFailure

