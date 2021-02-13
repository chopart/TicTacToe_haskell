{-# LANGUAGE TypeFamilies,FlexibleContexts,TemplateHaskell #-}
module TicTacToeTest where

import Test.QuickCheck
import Test.QuickCheck.All
import Game
import TicTacToe
import Data.Maybe (fromJust)
import Data.Map (fromListWith,toList)
import Data.List (nub, elem)

anyLength = choose (1,9) :: Gen Int

prop_tictactoeMoveCount  = forAll anyLength $ \k ->
                            forAll anyLength $ \m ->
                            forAll anyLength $ \n -> let 
                              g = TicTacToe k m n 
                              j = min (k-1) (m*n)
                              in and $ zipWith (==) (take j (map length $ firstMoves g True (startState g))) (iterate (\x -> x-1) (m*n))
                  
firstMoves g p s = case ms of
     []     -> []
     (m:_)  -> ms : firstMoves g (not p) (move g p s m)
  where ms = moves g p s

prop_boardDimensions  =   forAll anyLength $ \k ->
                          forAll anyLength $ \m ->
                          forAll anyLength $ \n -> let 
                            g = TicTacToe k m n
                            s = startState g
                            r = showGame g s
                          in (length $ lines r) == n && (nub (map length (lines r))) == [m]

prop_boardSymbols =  forAll anyLength $ \k ->
                     forAll anyLength $ \m ->
                     forAll anyLength $ \n -> let 
                       g = TicTacToe k m n
                       s = startState g
                       mv = head $ moves g True s
                       s1 = move g True s mv
                       s2 = move g False s mv
                     in 
                       (nub (concat $ lines $ showGame g s) == ".") &&
                       ('X' `elem` (showGame g s1)) && 
                       ('O' `elem` (showGame g s2))

{-
Hvis moves = [] skal value give 0,minBound eller maxBound.
-}

leafValues' game player state = case (moves game player state) of
  [] -> [value game player state ]
  ms -> concat $ [ leafValues' game (not player) (move game player state m) | m <- ms ]
leafValues game player = toList $ fromListWith (+) [(c, 1) | c <- leafValues' game player (startState game)]

prop_leafValuesAreCorrect = and [
    leafValues (TicTacToe 2 1 1) True == [(0,1)],
    leafValues (TicTacToe 2 1 2) True == [(0,2)],
    leafValues (TicTacToe 2 1 3) True == [(0,2),(maxBound,4)],
    leafValues (TicTacToe 2 1 4) True == [(minBound,4),(0,8),(maxBound,12)],
    leafValues (TicTacToe 2 1 5) True == [(minBound,24),(0,12),(maxBound,60)],
    leafValues (TicTacToe 2 2 1) True == [(0,2)],
    leafValues (TicTacToe 2 2 2) True == [(maxBound,24)],
    leafValues (TicTacToe 2 2 3) True == [(minBound,80),(maxBound,120)],
    leafValues (TicTacToe 2 2 4) True == [(minBound,416),(maxBound,1408)]
  ]

return []
main = $quickCheckAll
