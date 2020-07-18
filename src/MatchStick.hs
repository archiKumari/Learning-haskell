module MatchStick where

import Data.List
import Data.Char (isDigit)
import Data.Maybe
import Types
import MatchStickLogic

isValidInput :: [String] -> Bool
isValidInput [num1,op1,num2,"=",res] = and result
  where result = [ isValidNum num1
                 , isValidOperator op1
                 , isValidNum num2
                 , isValidNum res]
isValidInput _ = False

isValidNum :: String -> Bool
isValidNum str = case stringToNumber str of
  Just _ -> True
  _ -> False

isValidOperator :: String -> Bool
isValidOperator str = case stringToOperator str of
  Just _ -> True
  _ -> False

groupedExp :: String -> [String]
groupedExp str = groupBy (\ a b -> isDigit a && isDigit b) (filter (/=' ') str)

charToDigit :: Char -> Maybe Int
charToDigit '0' = Just 0
charToDigit '1' = Just 1
charToDigit '2' = Just 2
charToDigit '3' = Just 3
charToDigit '4' = Just 4
charToDigit '5' = Just 5
charToDigit '6' = Just 6
charToDigit '7' = Just 7
charToDigit '8' = Just 8
charToDigit '9' = Just 9
charToDigit _   = Nothing

stringToOperator :: String -> Maybe (Int->Int->Int)
stringToOperator "+" = Just (+)
stringToOperator "-" = Just (-)
stringToOperator "*" = Just (*)
stringToOperator _   = Nothing

stringToNumber :: String -> Maybe Int
stringToNumber str
 | elem Nothing digitStr = Nothing
 | otherwise = Just (toNumber digitStr)
  where
   digitStr =  map charToDigit str
   toNumber list = sum $ zipWith (*) (reverse $ map (\(Just x) -> x) list) ([10^x|x<-[0..]])

strToOperator :: String -> Operator
strToOperator str = case str of
  "+" -> Plus
  "-" -> Minus
  _  -> InvalidOperator

intToDigit :: Int -> Digit
intToDigit 0 = d0
intToDigit 1 = d1
intToDigit 2 = d2
intToDigit 3 = d3
intToDigit 4 = d4
intToDigit 5 = d5
intToDigit 6 = d6
intToDigit 7 = d7
intToDigit 8 = d8
intToDigit 9 = d9
intToDigit _ = DigitError

strToDigit :: String -> Digit
strToDigit str = intToDigit.fromJust $ stringToNumber str

stringToExpression :: String -> Expression
stringToExpression str 
 | isValidInput expGroup = Exp digit1 operator digit2 result
 | otherwise = InvalidExpression
 where digit1 = strToDigit (expGroup!!0)
       operator = strToOperator (expGroup!!1)
       digit2 = strToDigit (expGroup!!2)
       result = strToDigit (expGroup!!4)
       expGroup = groupedExp str

solveExpression :: String -> String
solveExpression str = unlines [inputExpStr , replicate 30 '-',outputStr]
  where
    outputStr = case solveMSExpression exp of
      Just solution -> show solution
      Nothing -> "The expression cannot be solved in one move!"
    exp = stringToExpression str
    inputExpStr = show exp

