{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module TicTacToe where
import Game
import Data.Bits
import qualified Data.Map as Map
type Length = Int
type Width = Int
type Height = Int

data TicTacToe = TicTacToe Length Width Height
instance Game TicTacToe where
  type GameState TicTacToe = Map.Map (Int, Int) Player
  type Move TicTacToe = (Int,Int)
  
  showGame (TicTacToe leng width height) s = insertAtEvery width '\n' $ (map (convert_coord s) (board width height))

  startState (TicTacToe leng width height) = Map.empty

  move g p s (h,w) = Map.insert (h,w) p s

  moves g@(TicTacToe leng width height) _ s 
    | board_wipe g True s = []
    | board_wipe g False s = []
    | otherwise = filter (find_nothing s) (board width height)
  
  value g p s 
    | board_wipe g True s == True = maxBound::Int
    | board_wipe g False s == True = minBound::Int
    | moves g p s == [] = 0
    | otherwise = (bloom_calc g True s 0 (0,0)) - (bloom_calc g False s 0 (0,0))

board width height = [(row,column) | row <- [0..width-1], column <- [0..height-1]]

--these states served as my help to test some of my subfunctions
g0 = TicTacToe 3 3 3
s0 = startState g0 
a = move g0 True s0 (0,0)
b = move g0 False (a) (0,1)
c = move g0 True (b) (0,2)
d = move g0 False (c) (1,0)
e = move g0 True (d) (1,1)
f = move g0 False (e) (1,2)
h = move g0 True (f) (2,0)
i = move g0 False (h) (2,1)
j = move g0 True (i) (2,2)

--checks a branch (vector) to see if there is a winner
branch_check g@(TicTacToe leng width height) p s c (d1,d2) (x,y)
    | Map.lookup (x,y) s /= Just p = False    
    | c == (leng-1) = True
    | valid_move (x+d1,y+d2) g == True && branch_check g p s (c+1) (d1,d2) (x+d1,y+d2) == True = True   
    | otherwise = False

--if there is no winner this calculates the score for a branch (vector)
branch_calc g@(TicTacToe leng width height) p s c (d1,d2) (x,y)
    | Map.lookup (x,y) s /= Just p && Map.lookup (x,y) s /= Nothing = 0
    | c == (leng-1) = 1
    | valid_move (x+d1,y+d2) g == True && branch_calc g p s (c+1) (d1,d2) (x+d1,y+d2) == 1 = 1  
    | otherwise = 0

--calculates the final value for a state, lower is better for player false and vice versa
bloom_calc g p s c (x,y) = (branch_calc g p s c (0,1) (x,y)) + (branch_calc g p s c (1,1) (x,y)) + (branch_calc g p s c (1,0) (x,y)) + (branch_calc g p s c (1,-1) (x,y))

--bloom_check combines branch_check in such a way that it takes a coordinate and runs branch_check in four directions on that coordinate
bloom_check g p s c (x,y)
    | branch_check g p s c (0,1) (x,y) == True = True  
    | branch_check g p s c (1,1) (x,y) == True = True
    | branch_check g p s c (1,0) (x,y) == True = True
    | branch_check g p s c (1,-1) (x,y) == True = True
    | otherwise = False

--board_wipe maps bloom_check to every coordinate on the board
board_wipe g@(TicTacToe leng width height) p s = or $ map (bloom_check g p s 0) $ board width height

--valid_move tjekker om koordinatet ligger inden for brættet
valid_move (x,y) (TicTacToe leng width height)
    | elem (x,y) (board width height) == True = True
    | otherwise = False

--helping function to find all the empty tiles on the board
find_nothing s (x,y)
    | Map.lookup (x,y) s == Nothing = True
    | Map.lookup (x,y) s /= Nothing = False

--helping function to convert the cords into X's ans O' and .'s
convert_coord s (x,y)
    | Map.lookup (x,y) s == Nothing = '.'
    | Map.lookup (x,y) s == Just True = 'X'
    | Map.lookup (x,y) s == Just False = 'O'

--helping function for sideeffects
insertAtEvery n y xs = countdown n xs
    where
        countdown 0 xs = y:countdown n xs --reset to original n
        countdown _ [] = []
        countdown m (x:xs) = x:countdown (m-1) xs
