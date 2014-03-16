maxSurpCount :: Ord a => [a] -> Int
maxSurpCount s = maximum $ map snd $ surpassers [] s
	where
	surpassers :: Ord a => [(a,Int)] -> [a] -> [(a,Int)]
	surpassers l [] = l
	surpassers [] (h:t) = surpassers [(h,0)] t
	surpassers l@((a,_):_) (h:t) | h < a = surpassers ((h,0):l) t
	surpassers l (h:t) = surpassers (surpass l h) t
		where
		surpass :: Ord a => [(a,Int)] -> a -> [(a,Int)]
		surpass ((a,n):l) h | h > a = (a,n+1):(surpass l h)
		surpass l _ = l
