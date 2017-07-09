module Main where

import Helpers
import Data.List
import Data.Char
import System.IO

data DrawData = DrawData {
  pos   :: Point,
  ps    :: [Point],
  color :: Color
} deriving (Show)

main :: IO ()
main = do
  let start = (0,0)
  setColor 37 40
  clear
  move start

  mainLoop DrawData { pos = start, ps = [], color = (37,40) }

mainLoop :: DrawData -> IO ()
mainLoop dd = do
  hSetEcho stdout False
  action <- getChar
  dd' <- actionHandler [action] dd

  let putStrOnPoint'  = putStrOnPoint (pos dd')
  let ps'   = zip (ps dd') (replicate (length (ps dd')) "█")
  clear
  mapM_ putStrOnPoint' ps'

  mainLoop dd'

up    = ["k", "\ESC[A"]
down  = ["j", "\ESC[B"]
left  = ["h", "\ESC[D"]
right = ["l", "\ESC[C"]

actionHandler :: String -> DrawData -> IO DrawData
actionHandler c dd
  | c == "\n" = if pos' `elem` ps'
                   then return dd { ps = delete pos' ps' }
                   else return dd { ps = pos' : ps' }
  | c == "c" = do
    sequence_ (clear : [move pos'])
    return dd { ps = [] }
  | c `elem` up = do
    p <- moveUp pos'
    return dd { pos = p }
  | c `elem` down = do
    p <- moveDown pos'
    return dd { pos = p }
  | c `elem` left = do
    p <- moveLeft pos'
    return dd { pos = p }
  | c `elem` right = do
    p <- moveRight pos'
    return dd { pos = p }
  | otherwise = return dd
  where
    pos'  = pos dd
    ps'   = ps dd
