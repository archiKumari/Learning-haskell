module StringExpression where

import Data.List
import Data.Char (isDigit)

 
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

groupedExp :: String -> [String]
groupedExp str = groupBy (\ a b -> isDigit a && isDigit b) (filter (/=' ') str)

checkEquation :: String -> Either String Bool
checkEquation str = case isValidInput (groupedExp str) of
  True -> case isCorrect str of
    True -> Right True
    False -> Right False
  _ -> Left "Invalid Input!"

isCorrect :: String -> Bool
isCorrect str
 |strToExp (take 3 expList) == strToExp (drop 3 expList) = True
 |otherwise = False
  where expList = groupedExp str

strToExp :: [String] -> Int
strToExp str
 |head str == "=" = jTof $ stringToNumber (last str)
 |otherwise = f num1 num2
  where f = jTof (stringToOperator $ str!!1)
        num1 = jTof $ stringToNumber (str!!0)
        num2 = jTof $ stringToNumber (str!!2)
  
jTof :: (Maybe a) -> a
jTof (Just a) = a

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

printExp :: String -> IO ()
printExp str = putStr.unlines $ expStr. fmap displayStr $ (:[]) <$> str

expStr :: (Monoid a) => [[a]] -> [a]
expStr [x] = x
expStr (x:y:ys) = expStr (r:ys)
  where r = zipWith (<>) x y

displayStr :: String -> [String]
displayStr "0" = [" @@@"," @ @"," @ @"," @ @"," @@@"]
displayStr "1" = ["   @","   @","   @","   @","   @"]
displayStr "2" = [" @@@","   @"," @@@"," @  "," @@@"]
displayStr "3" = [" @@@","   @"," @@@","   @"," @@@"]
displayStr "4" = [" @ @"," @ @"," @@@","   @","   @"]
displayStr "5" = [" @@@"," @  "," @@@","   @"," @@@"]
displayStr "6" = [" @@@"," @  "," @@@"," @ @"," @@@"]
displayStr "7" = [" @@@","   @","   @","   @","   @"]
displayStr "8" = [" @@@"," @ @"," @@@"," @ @"," @@@"]
displayStr "9" = [" @@@"," @ @"," @@@","   @"," @@@"]
displayStr "+" = ["      ","   @  "," @@@@@","   @  ","      "]
displayStr "-" = ["      ","      "," @@@@@","      ","      "]
displayStr "*" = [" @   @","  @ @ ","   @  ","  @ @ "," @   @"]
displayStr "/" = ["     @","    @ ","   @  ","  @   "," @    "]
displayStr "=" = ["      "," ====="," =====","      ","      "]
displayStr "right" = ["         @","        @ ","  @    @  ","   @  @","     @" ]
displayStr "wrong" = ["   @   @","    @ @ ","     @  ","    @ @ ","   @   @"]
displayStr _ = []

displayWithResult :: String -> String
displayWithResult str = case checkEquation str of
  Left x -> x
  Right True  -> unlines $ rightExp
  Right False -> "\BEL\n" <>  unlines wrongExp
 where
  rightExp = makeExp "right"
  wrongExp = makeExp "wrong"
  makeExp = zipWith (<>) (expStr. fmap displayStr $ (:[]) <$> str) . displayStr

