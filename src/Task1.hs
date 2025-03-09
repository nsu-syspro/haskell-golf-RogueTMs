module Task1 where

import Data.List (group)

-- | Compresses given data using run-length encoding.
--
-- Usage example:
--
-- >>> encode "aaabbccaadaaa"
-- [(3,'a'),(2,'b'),(2,'c'),(2,'a'),(1,'d'),(3,'a')]
-- >>> encode "abc"
-- [(1,'a'),(1,'b'),(1,'c')]
-- >>> encode []
-- []
--
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(length x, head x) | x <- group xs]

-- | Decompresses given data using run-length decoding.
--
-- Usage example:
--
-- >>> decode [(3,'a'),(2,'b'),(2,'c'),(2,'a'),(1,'d'),(3,'a')]
-- "aaabbccaadaaa"
-- >>> decode [(1,'a'),(1,'b'),(1,'c')]
-- "abc"
-- >>> decode []
-- []
--
decode :: [(Int, a)] -> [a]
decode xs = concat [replicate n x | (n,x) <- xs]

-- | Rotates given finite list to the left for a given amount N
--
-- If N is negative, then rotates to the right instead.
--
-- Usage example:
--
-- >>> rotate 3 "abcdefgh"
-- "defghabc"
-- >>> rotate (-2) "abcdefgh"
-- "ghabcdef"
-- >>> rotate 0 "abcdefgh"
-- "abcdefgh"
-- >>> rotate 5 "abc"
-- "cab"
-- >>> rotate 5 ""
-- ""
--
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = drop m xs ++ take m xs where m = mod n (length xs)
