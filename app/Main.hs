module Main where

import Data.Char
import Data.List
import Helpers
import System.IO

data DrawData = DrawData
  { pos :: Point
  , ps :: [Point]
  , color :: Color
  } deriving (Show)

main :: IO ()
main = do
  startPos <- getTermCenter
  let color = (37, 40)
  setColor color
  clear
  move startPos
  mainLoop DrawData {pos = startPos, ps = [], color = color}

mainLoop :: DrawData -> IO ()
mainLoop dd = do
  hSetEcho stdout False
  action <- getChar
  dd' <- actionHandler [action] dd
  let putStrOnPoint' = putStrOnPoint (pos dd')
  let ps' = zip (ps dd') (replicate (length (ps dd')) "█")
  clear
  mapM_ putStrOnPoint' ps'
  mainLoop dd'

up = ["k", "\ESC[A"]

down = ["j", "\ESC[B"]

left = ["h", "\ESC[D"]

right = ["l", "\ESC[C"]

actionHandler :: String -> DrawData -> IO DrawData
actionHandler c dd
  | c == "\n" =
    if pos' `elem` ps'
      then return dd {ps = delete pos' ps'}
      else return dd {ps = pos' : ps'}
  | c == "c" = return dd {ps = []}
  | c `elem` up = moveUp pos' >>= \p -> return dd {pos = p}
  | c `elem` down = moveDown pos' >>= \p -> return dd {pos = p}
  | c `elem` left = moveLeft pos' >>= \p -> return dd {pos = p}
  | c `elem` right = moveRight pos' >>= \p -> return dd {pos = p}
  | otherwise = return dd
  where
    pos' = pos dd
    ps' = ps dd
