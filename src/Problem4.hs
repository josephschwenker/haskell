-- Problem 4

-- Part (a).

type SStack a = [a]

push :: Ord a => a -> SStack a -> SStack a


pop :: Ord a => SStack a -> Maybe (a, SStack a)