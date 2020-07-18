module MatchStickLogic where

import Types
import Data.List
import Data.Maybe

isValidDigit :: Digit -> Bool
isValidDigit d = elem d validDigits

validDigits = [d0,d1,d2,d3,d4,d5,d6,d7,d8,d9]

stListToD :: [Status] -> Maybe Digit
stListToD sList
  | digitTuple /= [] = Just (head . map fst $ digitTuple)
  | otherwise = Nothing
 where digitTuple = filter (\x -> snd x == sList) digitStatusTuple

digitStatusTuple = map (\x->(x,dToStList x)) validDigits

operatorToFunction :: (Num a) => Operator -> (a->a->a)
operatorToFunction Plus  = (+)
operatorToFunction Minus = (-)

digitToInt :: Digit -> Int
digitToInt d
 | d == d0 = 0
 | d == d1 = 1
 | d == d2 = 2
 | d == d3 = 3
 | d == d4 = 4
 | d == d5 = 5
 | d == d6 = 6
 | d == d7 = 7
 | d == d8 = 8
 | d == d9 = 9

reOrder :: Digit -> [Digit]
reOrder d 
  | digitList /= [] = digitList
  | otherwise = []
 where digitList = filter correct.fmap fromJust $ filter isJust (fmap stListToD (nub . permutations $ dToStList d))
       correct x = checkOneInterchange (dToStList d) (dToStList x)

modifyDigit :: Digit -> Status -> [Digit]
modifyDigit d find = fmap fromJust validStatusList
 where validStatusList = filter isJust $ map stListToD editedList
       editedList = zipWith3 replace indexList (repeat replaceBy) (repeat dStatusList)
       indexList = findIndices (==find) dStatusList
       dStatusList = dToStList d
       replaceBy = if find == ON then OFF else ON

removeMS :: Digit -> [Digit]
removeMS d = modifyDigit d ON

removeInOp :: Operator -> [Operator]
removeInOp Plus = [Minus]
removeInOp _ = []

insertMS :: Digit -> [Digit]
insertMS d = modifyDigit d OFF

insertInOp :: Operator -> [Operator]
insertInOp Minus = [Plus]
insertInOp _ = []

replace :: Int -> a -> [a] -> [a]
replace n k list = (fst listSplit) ++ [k] ++ (drop 1 $ snd listSplit)
 where listSplit = splitAt n list

checkMsEquation :: Expression -> Bool
checkMsEquation (Exp d1 op d2 res) 
 |f (digitToInt d1) (digitToInt d2) == (digitToInt res) = True
 |otherwise = False
 where f = operatorToFunction op

solveByReOrder :: Expression -> [Expression]
solveByReOrder exp@(Exp dt1 op dt2 res) = concat solutions
  where
    solutions  = zipWith fn [1,2,3] [dt1, dt2, res]
    fn n d = filter checkMsEquation $ expWithDigits exp n (reOrder d)

solveByReOrder' :: Expression -> [Expression]
solveByReOrder' exp@(Exp d1 op d2 res) = 
  case reOrder d1 of
    digits | [] /= r -> r
      where r = filter checkMsEquation $ expWithDigits exp 1 digits
    _   -> case reOrder d2 of
      digits | [] /= s -> s
        where s = filter checkMsEquation $ expWithDigits exp 2 digits
      _ -> case reOrder res of
        digits | [] /= t -> t
          where t = filter checkMsEquation $ expWithDigits exp 3 digits
        _ -> []

changeExp :: Int -> Expression -> Digit -> Expression
changeExp n exp@(Exp d1 op d2 res) d = case n of
  1 -> Exp d op d2 res
  2 -> Exp d1 op d res
  3 -> Exp d1 op d2 d
  _ -> exp

changeOP :: Expression -> Expression
changeOP (Exp d1 Plus d2 res) = Exp d1 Minus d2 res
changeOP (Exp d1 Minus d2 res) = Exp d1 Plus d2 res

expWithDigits :: Expression -> Int -> [Digit] -> [Expression]
expWithDigits exp n digits = case n of
 1 -> fmap (changeExp 1 exp) digits
 2 -> fmap (changeExp 2 exp) digits
 3 -> fmap (changeExp 3 exp) digits

modifyExp :: [Expression] -> Int -> [Digit] -> [[Expression]]
modifyExp [] _ _ = []
modifyExp (e:es) n digitList = case n of
 1 -> expWithDigits e 1 digitList : modifyExp es 1 digitList
 2 -> expWithDigits e 2 digitList : modifyExp es 2 digitList
 3 -> expWithDigits e 3 digitList : modifyExp es 3 digitList

solveByModify :: Expression -> [Expression]
solveByModify exp@(Exp d1 op d2 res) =
  case (removeInsert exp 1) of
   [] -> case (removeInsert exp 2) of
     [] -> case (removeInsert exp 3) of
       [] -> case (removeInOp op) of
         [] -> []
         list -> removeInsert exp 4
       list -> list
     list -> list
   list -> list

removeInsert
  :: Expression
  -> Int
  -> [Expression]
removeInsert exp@(Exp dt1 op dt2 res) n =
  case r of
   (l:ls) -> r
   [] -> case s of
     (l:ls) -> s
     [] -> case eOP op of
       [] -> []
       (l:ls)  -> case t of 
          (l:ls) -> t
          [] -> []
  where 
    r = case n of
      1 -> validEquations.concat $ modifyExp (expWithDigits exp 1 (removeMS dt1)) 2 (insertMS dt2)
      2 -> validEquations.concat $ modifyExp (expWithDigits exp 2 (removeMS dt2)) 3 (insertMS res)
      3 -> validEquations.concat $ modifyExp (expWithDigits exp 3 (removeMS res)) 1 (insertMS dt1)
      4 -> validEquations $ map changeOP (expWithDigits exp 1 (insertMS dt1))
    s = case n of
      1 -> validEquations.concat $ modifyExp (expWithDigits exp 1 (removeMS dt1)) 3 (insertMS res)
      2 -> validEquations.concat $ modifyExp (expWithDigits exp 2 (removeMS dt2)) 1 (insertMS dt1)
      3 -> validEquations.concat $ modifyExp (expWithDigits exp 3 (removeMS res)) 2 (insertMS dt2)
      4 -> validEquations $ map changeOP (expWithDigits exp 2 (insertMS dt2))
    t = case n of
      1 -> validEquations $ map changeOP (expWithDigits exp 1 (removeMS dt1))
      2 -> validEquations $ map changeOP (expWithDigits exp 2 (removeMS dt2))
      3 -> validEquations $ map changeOP (expWithDigits exp 3 (removeMS res))
      4 -> validEquations $ map changeOP (expWithDigits exp 3 (insertMS res))
    validEquations = filter checkMsEquation
    eOP = if n==4 then removeInOp else insertInOp

solveMSExpression :: Expression -> Maybe Expression
solveMSExpression exp =
  case solveByReOrder exp of
    (x:xs) -> Just x
    [] -> case solveByModify exp of
      (x:xs) -> Just x
      [] -> Nothing

deepakModification :: Expression -> Maybe Expression
deepakModification exp = case allValidCombinations exp of
  x : xs -> Just x
  _      -> Nothing
  where
    allValidCombinations e@(Exp a1 o a2 a3) = concat $ applyFunc e <$> flagCombinations
    applyFunc (Exp a b c d) [flag1,flag2,flag3,flag4] =
      filter checkMsEquation [Exp a1 a2 a3 a4 | a1 <- f1 a, a2 <- f2 b, a3 <- f3 c, a4 <- f4 d]
      where
        (f1, f2, f3, f4) = (dChange flag1, opChange flag2, dChange flag3, dChange flag4)
        dChange  flag = case flag of
          1 -> insertMS
          2 -> removeMS
          _ -> pure

        opChange flag = case flag of
          1 -> insertInOp
          2 -> removeInOp
          _ -> pure

    flagCombinations = nub . permutations $ [0,0,1,2]

checkOneInterchange :: Eq a => [a] -> [a] -> Bool
checkOneInterchange l1 l2
  | length l1 /= length l2 = False
  | otherwise =
      case filter (\(a,b) -> a /= b) $ zip l1 l2 of
        [(a,b),(c,d)] -> a == d && b == c
        _             -> False

