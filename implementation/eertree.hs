{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Data.List (find)

data Trie a = Trie [a] [(a, Trie a)]
    deriving Show

data Palindrome a = Palindrome [a] [a] Parity

data Parity = Even | Odd

data Eertree a = Eertree
    { oddTrie  :: Trie a,
      evenTrie :: Trie a,
      maxSuff  :: [a],
      revPref  :: [a]
    } deriving Show

data DirectLinks a = DirectLinks
  { dlMaxSuf  :: [a]
  , dlRevPref :: [a]
  , dlLinks   :: [(a, DirectLinks a)]
  } deriving (Show)

empty :: Eertree a
empty = Eertree
    { oddTrie = Trie [] [],
      evenTrie = Trie [] [],
      maxSuff = [],
      revPref = []
    }

addPath :: Eq a => [a] -> Trie a -> Trie a
addPath [] t = t
addPath [c] t@(Trie s ts)
  | c `elem` map fst ts = t
  | otherwise           = Trie s ((c, Trie (c : s ++ [c]) []) : ts)
addPath (c:cs) (Trie s ts) = Trie s (map g ts)
  where
    g edge@(c', t)
      | c == c'   = (c', addPath cs t)
      | otherwise = edge

triePalindromes :: (a -> b -> b) -> b -> Trie a -> [b]
triePalindromes f z (Trie _ ts) = z :
  [ f c p
  | (c, t) <- ts
  , p <- triePalindromes f z t ]

insertCenter :: a -> Palindrome a -> Palindrome a
insertCenter c (Palindrome l r parity)
  = Palindrome (l ++ [c]) (c : r) parity

fromPalindrome :: Palindrome a -> [a]
fromPalindrome (Palindrome l r parity) =
  case parity of
    Even -> l ++ r
    Odd  -> l ++ drop 1 r

oddTriePalindromes :: Trie a -> [Palindrome a]
oddTriePalindromes = triePalindromes insertCenter (Palindrome [] [] Odd)

evenTriePalindromes :: Trie a -> [Palindrome a]
evenTriePalindromes = triePalindromes insertCenter (Palindrome [] [] Even)

subpalindromes :: Eq a => [a] -> [[a]]
subpalindromes
  = filter (not . null)
  . map fromPalindrome
  . pals
  . eertree
  where
    pals Eertree {..}
      = oddTriePalindromes oddTrie
      ++ evenTriePalindromes evenTrie

candidates :: ([a], [a]) -> [([a], [a])]
candidates (pref, [])     = [(pref, [])]
candidates (pref, c:suff) = (pref, c:suff) : candidates (c:pref, suff)


isPalindrome :: Eq a => [a] -> Bool
isPalindrome s = s == reverse s


suffCandidates :: Eq a => Eertree a -> [([a], [a])]
suffCandidates Eertree{..} = filter g (candidates (revPref, maxSuff))
    where
        g (_, suff) = isPalindrome suff


findSuff :: Eq a => a -> Eertree a -> Maybe ([a], [a])
findSuff c t = find g (suffCandidates t)
    where
       g (c':_, _) = c' == c
       g _         = False



add :: Eq a => a -> Eertree a -> Eertree a
add c t = addPalindrome (findSuff c t)
  where
    addPalindrome Nothing = t
      { oddTrie = addPath [c] (oddTrie t),
        maxSuff = [c],
        revPref = maxSuff t ++ revPref t
      }
    addPalindrome (Just (pref, suff)) = t
      { oddTrie = (if isEven then id else addPalPath) (oddTrie t),
        evenTrie = (if isEven then addPalPath else id) (evenTrie t),
        maxSuff = c:suff ++ [c],
        revPref = tail pref
      }
      where
        isEven = length suff `mod` 2 == 0
        halfSuff = drop (length suff `div` 2) suff
        addPalPath = addPath (halfSuff ++ [c])

eertree :: Eq a => [a] -> Eertree a
eertree = foldl (flip add) empty

a216264 :: [Int]
a216264 =
  [ k
  | n <- [1..]
  , let k = length
          [ s
          | s <- binaryStrings n
          , length (subpalindromes s) == n ]
  ]
    where
      binaryStrings 0 = [[]]
      binaryStrings n = concat
        [ [0:s, 1:s]
        | s <- binaryStrings (n - 1) ]

merge :: Eq a => Eertree a -> Eertree a -> Eertree a
merge e1 e2 = foldl (flip add) e1 seq
    where
        seq = reverse (revPref e2) ++ (maxSuff e2)

set :: Eq a => Eertree a -> Int -> a -> Eertree a
set e n c = eertree seq
    where
        tmpSeq = reverse (revPref e) ++ (maxSuff e)
        seq = a ++ (c:b) where (a, (_:b)) = splitAt n tmpSeq
