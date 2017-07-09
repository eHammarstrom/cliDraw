module Main where

import Helpers
import Data.List
import Data.Char
import System.IO

main :: IO ()
main = do
  let start = (0,0)
  setColor 37 40
  clear
  move start

  mainLoop start []

mainLoop :: Point -> [Point] -> IO ()
mainLoop pos ps = do
  hSetEcho stdout False
  action <- getChar
  (newPos, newPs) <- actionHandler [action] pos ps

  let put'  = put newPos
  let ps'   = zip newPs (replicate (length newPs) "█")
  clear
  mapM_ put' ps'

  mainLoop newPos newPs

up    = ["k", "\ESC[A"]
down  = ["j", "\ESC[B"]
left  = ["h", "\ESC[D"]
right = ["l", "\ESC[C"]

actionHandler :: String -> Point -> [Point] -> IO (Point, [Point])
actionHandler c pos ps
  | c == "\n" = if pos `elem` ps
                   then return (pos, delete pos ps)
                   else return (pos, pos : ps)
  | c == "c" = do
    sequence_ (clear : [move pos])
    return (pos, [])
  | c `elem` up = do
    p <- moveUp pos
    return (p, ps)
  | c `elem` down = do
    p <- moveDown pos
    return (p, ps)
  | c `elem` left = do
    p <- moveLeft pos
    return (p, ps)
  | c `elem` right = do
    p <- moveRight pos
    return (p, ps)
  | otherwise = return (pos, ps)
