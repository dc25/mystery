import Data.List (unfoldr)

class Eq a => Gen a where
    next :: a -> a

gen :: Gen a => a -> a -> [a]
gen from to =
    unfoldr
        (\b -> if b == to then Nothing else Just (b, next b))
        from

mystery :: Gen a => [a] -> [a]
mystery (x : xr) = go (next x) xr
    where
        go curr (y : ys)
            = gen curr y ++ go (next y) ys
        go _ _ = []
mystery [] = []

instance Gen Int where
    next = (+) 1

main :: IO ()
main = do
    print $ mystery ([2, 14, 15, 16, 19] :: [Int])
    print $ mystery ([2, 6] :: [Int])
    print $ mystery ([2, 5] :: [Int])
    print $ mystery ([2, 4] :: [Int])
    print $ mystery ([2, 3] :: [Int])
    print $ mystery ([2] :: [Int])
    print $ mystery ([] :: [Int])
