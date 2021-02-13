maximumSnd = maximumBy (comparing snd)
minimumSnd = minimumBy (comparing snd)

minleaf :: Game g => g -> GTree g -> (Maybe (Move g), Value)
minleaf g (Tree (p,s) []) = (Nothing, value g p s)
minleaf g (Tree (p,s) c) = minimumSnd [(Just m, snd $ maxleafs g t) | (m,t) <- c]

maxleaf :: Game g => g -> GTree g -> (Maybe (Move g), Value)
maxleaf g (Tree (p,s) []) = (Nothing, value g p s)
maxleaf g (Tree (p,s) c) = maximumSnd [(Just m, snd $ minleafs g t) | (m,t) <- c]

maxNodeDepth :: Game g => g -> Depth -> AlphaBeta -> GTree g -> (Maybe (Move g), Value)
maxNodeDepth g _ (a,b) (Tree (p,s) []) = (Nothing,value g p s)
maxNodeDepth g 0 (a,b) (Tree (p,s) _) = (Nothing,value g p s)
maxNodeDepth g _ (a,b) (Tree (p,s) []) = maximumSnd [(Just m, snd $ minNodeDepth g t) | (m,t) <- c]

minNodeDepth :: Game g => g -> Depth -> AlphaBeta -> GTree g -> (Maybe (Move g), Value)
minNodeDepth g _ (a,b) (Tree (p,s) []) = (Nothing,value g p s)
minNodeDepth g 0 (a,b) (Tree (p,s) _) = (Nothing,value g p s)
minNodeDepth g _ (a,b) (Tree (p,s) []) = minimumSnd [(Just m, snd $ maxNodeDepth g t) | (m,t) <- c]
