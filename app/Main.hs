module Main where

import Types
import Tree
import MatchStick
import MatchStickLogic
import MatchStickUtilities
import System.Random
import Data.Maybe
import Control.Concurrent
import System.Environment

main :: IO ()
main = do
 arg <- getArgs
 case arg of
   [] -> print "Please provide proper argument ! Specify Game Time in Minutes"
   t : name : ts -> do
     _   <- forkIO $ timer (read t)
     game name 0


timer :: Int -> IO ()
timer time = do
  threadDelay (time * 60000000)
  print $ "Oops Time Up! Game Over"


game :: String -> Int -> IO ()
game name score = do
  putStrLn "****MATCHSTICK PUZZLE***"
  exp <- genMsPuzzle
  print exp
  displayScore name score
  print "Give your answer for the puzzle:"
  gameLoop name score exp

gameLoop :: String -> Int -> Expression -> IO ()
gameLoop name score exp = do
  userAns <- getLine
  case userAns of
    "q" -> do
      displayScore name score
      print "Byee Byee .... Game Over"
    "a" -> do
      putStrLn "Solution"
      print $ fromJust $ solveMSExpression exp
      game name score
    _ -> do
      let maybeExp = stringToExpression userAns
      case maybeExp of
        InvalidExpression -> do
          print "Input is not in correct Format!"
          tryAgain
          gameLoop name score exp

        ansExp | isSolution ansExp exp -> do
          success
          print ansExp
          game name (score + 10)

        wrongSolution -> do
          failure
          tryAgain
          gameLoop name score exp


displayScore :: String -> Int -> IO ()
displayScore name x = do 
  putStrLn $ "Player Name : " <> name
  putStrLn $ "Score : " <> show x

tryAgain :: IO ()
tryAgain = print "Please try again"

success :: IO ()
success = putStrLn " Congratulations!! You Got it....... Correct  Answer! "

failure :: IO ()
failure = print " Ohhh No .... Wrong Answer!"

-- Function to generate valid MatchStick Puzzle
genMsPuzzle :: IO Expression
genMsPuzzle = do
  n <- randomRIO (0,100)
  opRvalue <- randomRIO (True,False)
  let [dt1,dt2,dt3] = fmap intToDigit.take 3 $ randomRs (0,9) (mkStdGen n)
      op = if opRvalue == True then Plus else Minus
      exp = Exp dt1 op dt2 dt3
  case getAllSolutions exp of
    x:xs | not $ checkMsEquation exp -> return exp
    _ -> genMsPuzzle

