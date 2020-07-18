{-# LANGUAGE InstanceSigs #-}

module Types where

import Data.List

data Digit = 
  D0 Status Status Status Status Status Status Status
  | D1 Status Status Status Status Status Status Status
  | D2 Status Status Status Status Status Status Status
  | D3 Status Status Status Status Status Status Status
  | D4 Status Status Status Status Status Status Status
  | D5 Status Status Status Status Status Status Status
  | D6 Status Status Status Status Status Status Status
  | D7 Status Status Status Status Status Status Status
  | D8 Status Status Status Status Status Status Status
  | D9 Status Status Status Status Status Status Status
  | DigitError
  deriving (Eq)

instance Show Digit where
  show = unlines . makeDigitStr

makeDigitStr :: Digit -> [String]
makeDigitStr d =
  let [a1,a2,a3,a4,a5,a6,a7] = zipWith stToStr (dToStList d) [1..]
  in [a1,a2 ++ a3, a4,a5 ++ a6,a7]

dToStList :: Digit -> [Status]
dToStList (D0 s1 s2 s3 s4 s5 s6 s7) = [s1,s2,s3,s4,s5,s6,s7]
dToStList (D1 s1 s2 s3 s4 s5 s6 s7) = [s1,s2,s3,s4,s5,s6,s7]
dToStList (D2 s1 s2 s3 s4 s5 s6 s7) = [s1,s2,s3,s4,s5,s6,s7]
dToStList (D3 s1 s2 s3 s4 s5 s6 s7) = [s1,s2,s3,s4,s5,s6,s7]
dToStList (D4 s1 s2 s3 s4 s5 s6 s7) = [s1,s2,s3,s4,s5,s6,s7]
dToStList (D5 s1 s2 s3 s4 s5 s6 s7) = [s1,s2,s3,s4,s5,s6,s7]
dToStList (D6 s1 s2 s3 s4 s5 s6 s7) = [s1,s2,s3,s4,s5,s6,s7]
dToStList (D7 s1 s2 s3 s4 s5 s6 s7) = [s1,s2,s3,s4,s5,s6,s7]
dToStList (D8 s1 s2 s3 s4 s5 s6 s7) = [s1,s2,s3,s4,s5,s6,s7]
dToStList (D9 s1 s2 s3 s4 s5 s6 s7) = [s1,s2,s3,s4,s5,s6,s7]

operatorToStatus :: Operator -> [Status]
operatorToStatus Plus = [ON,ON]
operatorToStatus Minus = [OFF,ON]

data Status = ON | OFF deriving (Eq,Show)

d0,d1,d2,d3,d4,d5,d6,d7,d8,d9 :: Digit
d0 = D0 ON ON ON OFF ON ON ON
d1 = D1 OFF OFF ON OFF OFF ON OFF
d2 = D2 ON OFF ON ON ON OFF ON
d3 = D3 ON OFF ON ON OFF ON ON
d4 = D4 OFF ON ON ON OFF ON OFF
d5 = D5 ON ON OFF ON OFF ON ON
d6 = D6 ON ON OFF ON ON ON ON
d7 = D7 ON OFF ON OFF OFF ON OFF
d8 = D8 ON ON ON ON ON ON ON
d9 = D9 ON ON ON ON OFF ON ON

stToStr s n = case s of
  ON  -> onString !! (n-1)
  OFF -> offString !! (n-1) 

onString = [" -- ","|","  |"," -- ","|","  |"," -- "]
offString= ["    "," ","   ","    "," ","   ","    "]

data Operator = Plus | Minus | InvalidOperator deriving (Eq)

instance Show Operator where
 show = unlines . makeOpStr

makeOpStr :: Operator -> [String]
makeOpStr Plus = ["     ","  |  ","-----","  |  ","     "]
makeOpStr Minus =["     ","     "," ----","     ","     "]

data Expression =
    Exp Digit Operator Digit Digit 
  | InvalidExpression
  deriving (Eq)

instance Show Expression where
 show :: Expression -> String
 show (Exp d1 op d2 res) = unlines $ (expString $ intersperse (repeat " ")  [makeDigitStr d1,makeOpStr op,makeDigitStr d2,eqStr,makeDigitStr res])
   where
     eqStr = ["      "," ====="," =====","      ","      "]

expString :: (Monoid a) => [[a]] -> [a]
expString [x] = x
expString (x:y:ys) = expString (r:ys)
  where r = zipWith (<>) x y
