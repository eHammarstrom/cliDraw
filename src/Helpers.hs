module Helpers(
  Point,
  put,
  move,
  moveUp,
  moveDown,
  moveLeft,
  moveRight,
  setColor,
  clear
  ) where

import Data.Char
import Data.List
-- import System.Console.Terminal.Size

type Point = (Int, Int)

put :: Point -> (Point, String) -> IO ()
put oldP (p, s) = do
  move p
  putStr s
  move oldP

move :: Point -> IO ()
move (x, y) = putStr (constructEscape (y : [x]) "H")

moveUp :: Point -> IO Point
moveUp (x, y) =
  if y-1 > 0 then do
    putStr (constructEscape [1] "A")
    return (x, y-1)
  else
    return (x, y)

moveDown :: Point -> IO Point
moveDown (x, y) = do
  putStr (constructEscape [1] "B")
  return (x, y+1)

moveLeft :: Point -> IO Point
moveLeft (x, y) =
  if x-1 > 0 then do
    putStr (constructEscape [1] "D")
    return (x-1, y)
  else
    return (x, y)

moveRight :: Point -> IO Point
moveRight (x, y) = do
  putStr (constructEscape [1] "C")
  return (x+1, y)

setColor :: Int -> Int -> IO ()
setColor fg bg = putStr (constructEscape (fg : [bg]) "m")

clear :: IO ()
clear = putStr (constructEscape [2] "J")

constructEscape :: [Int] -> String -> String
constructEscape args code = "\ESC[" ++ intercalate ";" (map show args) ++ code
