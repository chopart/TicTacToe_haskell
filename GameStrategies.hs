{-# LANGUAGE FlexibleContexts #-}
module GameStrategies where
import TicTacToe
import Data.List (intersperse, maximumBy, minimumBy)
import Data.Ord (comparing)
import Game
data Tree m v   = Tree v [(m,Tree m v)] 
type GTree g    = Tree (Move g) (Player,GameState g) 

type AlphaBeta = (Value,Value)
type Depth = Int
instance (Show m, Show v) => Show (Tree m v) where
  show (Tree x xs) = "Tree " ++ show x ++ " ["++ sub' ++"]"
    where 
      sub = concat $ intersperse ",\n" $ map show xs 
      sub' = if null sub 
                then "" 
                else "\n" ++ (unlines $ map ("  " ++ ) $ lines sub)  


startTree :: Game g => g -> Player -> GTree g
startTree g p = tree g (p, startState g)

tree :: Game g => g -> (Player, GameState g) -> GTree g
tree g (p,s) 
    | length mvs == 0 = Tree (p,s) []
    | otherwise = Tree (p,s) children
        where
            mvs = moves g p s
            sts = [move g p s m | m <- mvs]
            trs = [tree g (not p,st) | st <- sts]
            children = zip mvs trs

--this works now
minimax :: Game g => g -> GTree g -> (Maybe (Move g), Value)
minimax g t@(Tree (p,_) _) = if p then maxleaf g t
                                  else minleaf g t

minleaf :: Game g => g -> GTree g -> (Maybe (Move g), Value)
minleaf g (Tree (p,s) []) = (Nothing, value g p s)
minleaf g (Tree (p,s) c) = minimumSnd [(Just m, snd $ maxleaf g t) | (m,t) <- c]

maxleaf :: Game g => g -> GTree g -> (Maybe (Move g), Value)
maxleaf g (Tree (p,s) []) = (Nothing, value g p s)
maxleaf g (Tree (p,s) c) = maximumSnd [(Just m, snd $ minleaf g t) | (m,t) <- c]

maximumSnd :: Ord a => [(t, a)] -> (t, a)
maximumSnd []     = error "maximum of empty list"
maximumSnd (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n < (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps

minimumSnd :: Ord a => [(t, a)] -> (t, a)
minimumSnd []     = error "minimum of empty list"
minimumSnd (x:xs) = minTail x xs
  where minTail currentMin [] = currentMin
        minTail (m, n) (p:ps)
          | n > (snd p) = minTail p ps
          | otherwise   = minTail (m, n) ps
       
--pruning seems to be the problem
minimaxAlphaBeta :: Game g => g -> AlphaBeta -> GTree g -> (Maybe (Move g), Value)
minimaxAlphaBeta g _ t@(Tree (p,s) []) = (Nothing, value g p s)
minimaxAlphaBeta g (a,b) t@(Tree (p,s) c) = if p then maxLoop g c (a,b) (Nothing,minBound::Int)
                                                 else minLoop g c (a,b) (Nothing,maxBound::Int)

--look at pseudocode to find explantion :P 
--alpha pruning
minLoop g [] (a,b) (mov,val) = (mov,val)
minLoop g ((m,t):xs) (a,b) (mov,val)
    | newV <= a = (m',v') --this pruns the tree 
    | otherwise = minLoop g xs (a,b') (m',v')
        where 
            newV = min val $ snd $ minimaxAlphaBeta g (a,b) t 
            b' =  min newV b --updates the value 
            (m',v') = if newV < val then (Just m,newV) else (mov,val)  

--beta pruning
maxLoop g [] (a,b) (mov,val) = (mov,val)
maxLoop g ((m,t):xs) (a,b) (mov,val)
    | newV >= b = (m',v') 
    | otherwise = maxLoop g xs (a',b) (m',v')
        where 
            newV = max val $ snd $ minimaxAlphaBeta g (a,b) t  
            a' =  max newV a
            (m',v') = if newV > val then (Just m,newV) else (mov,val) 


--this needs pruning to work
minimaxAlphaBetaBounded :: Game g => g -> Depth -> AlphaBeta -> GTree g -> (Maybe (Move g), Value)
minimaxAlphaBetaBounded g d (a,b) t@(Tree (p,s) c) = if p then maxNodeDepth g d (a,b) t
                                                          else minNodeDepth g d (a,b) t

maxNodeDepth :: Game g => g -> Depth -> AlphaBeta -> GTree g -> (Maybe (Move g), Value)
maxNodeDepth g _ (a,b) (Tree (p,s) []) = (Nothing,value g p s)
maxNodeDepth g 0 (a,b) (Tree (p,s) _) = (Nothing,value g p s)
maxNodeDepth g d (a,b) (Tree (p,s) c) = maxDepthLoop g d c (a,b) (Nothing,minBound::Int) 

minNodeDepth :: Game g => g -> Depth -> AlphaBeta -> GTree g -> (Maybe (Move g), Value)
minNodeDepth g _ (a,b) (Tree (p,s) []) = (Nothing,value g p s)
minNodeDepth g 0 (a,b) (Tree (p,s) _) = (Nothing,value g p s)
minNodeDepth g d (a,b) (Tree (p,s) c) = minDepthLoop g d c (a,b) (Nothing,maxBound::Int) 

maxDepthLoop g d [] (a,b) (mov,val) = (mov,val)
maxDepthLoop g d ((m,t):xs) (a,b) (mov,val)
    | newV >= b = (m',v') 
    | otherwise = maxDepthLoop g d xs (a',b) (m',v')
        where 
            newV = max val $ snd $ minimaxAlphaBetaBounded g (d-1) (a,b) t  
            a' =  min newV b
            (m',v') = if newV > val then (Just m,newV) else (mov,val) 
 
minDepthLoop g d [] (a,b) (mov,val) = (mov,val)
minDepthLoop g d ((m,t):xs) (a,b) (mov,val)
    | newV <= a = (m',v') 
    | otherwise = minDepthLoop g d xs (a,b') (m',v')
        where 
            newV = min val $ snd $ minimaxAlphaBetaBounded g (d-1) (a,b) t  
            b' =  min newV b
            (m',v') = if newV < val then (Just m,newV) else (mov,val)  

t0 = startTree g0 True
