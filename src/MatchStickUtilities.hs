module MatchStickUtilities where

import Types
import MatchStickLogic
import Data.Maybe

expToStList :: Expression -> [Status]
expToStList (Exp dt1 op dt2 res) = concat $ [dToStList dt1, operatorToStatus op, dToStList dt2,dToStList res]

checkSolution :: Expression -> Expression -> Bool
checkSolution exp1 res = checkOneInterchange (expToStList exp1) (expToStList res)

allExpressions :: [Expression]
allExpressions = [Exp dt1 op dt2 dt3|dt1<-validDigits,op<-[Plus,Minus],dt2<-validDigits,dt3<-validDigits]

msPuzzles :: [Expression]
msPuzzles = filter ((/= Nothing) . solveMSExpression)  $ allExpressions

validMsPuzzlesWithSol :: [(Expression,Expression)]
validMsPuzzlesWithSol = fmap (fmap fromJust) $ filter ((/= Nothing) . snd) $ map (\exp -> (exp,solveMSExpression exp)) $ allExpressions

getAllSolutions :: Expression -> [Expression]
getAllSolutions exp = solveByReOrder exp ++ solveByModify exp

isSolution :: Expression -> Expression -> Bool
isSolution solution exp = elem solution (getAllSolutions exp)
