{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (Char)
import Data.List (tails, find)
import Data.Maybe (maybeToList)
import Control.Arrow (first, second)


data Trie a = Trie [a] [(a, Trie a)]
    deriving Show


data Eertree a = Eertree 
    { oddTrie :: Trie a,
      evenTrie :: Trie a,
      maxSuff :: [a],
      revPref :: [a]
    } deriving Show

                           
empty :: Eertree a
empty = Eertree 
    { oddTrie = Trie [] [],
      evenTrie = Trie [] [],
      maxSuff = [],
      revPref = []
    }


addPath :: Eq a => [a] -> Trie a -> Trie a
addPath (c:[]) (Trie s' path) = Trie s' (path ++ [(c, Trie (c:s' ++ [c]) [])]) 
addPath (c:s) (Trie s' path) = Trie s' (map (\(c', t') -> if c' == c then (c', addPath s t') else (c', t')) path) 


subpalindromes :: Eq a => [a] -> [[a]]
subpalindromes s = filter (/=[]) (len (eertree s))
    where
        len Eertree {..} = trieToList oddTrie ++ trieToList evenTrie
        trieToList :: Trie a -> [[a]]
        trieToList (Trie s path) = [s] ++ concat (map (trieToList . snd) path) 


candidates :: ([a], [a]) -> [([a], [a])]
candidates (pref, []) = [(pref, [])]
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
       g _ = False 



add :: Eq a => a -> Eertree a -> Eertree a
add c t =
    case findSuff c t of
        Nothing -> t {
                    oddTrie = addPath [c] (oddTrie t),
                    evenTrie = evenTrie t, 
                    maxSuff = [c], 
                    revPref = maxSuff t ++ revPref t 
                    }
        Just (pref, suff) -> t { 
                    oddTrie =
                        (case (length suff) `mod` 2 of
                            1 -> addPath (reverse (fst (splitAt ((length (suff) + 1) `div` 2) suff)) ++ [c]) (oddTrie t) 
                            0 -> oddTrie t),
                    evenTrie =
                        (case (length suff) `mod` 2 of
                            1 -> evenTrie t
                            0 -> addPath (reverse (fst (splitAt ((length (suff)) `div` 2) suff)) ++ [c]) (evenTrie t)),
                    maxSuff = c:suff ++ [c], 
                    revPref = tail pref 
                    } 

eertree :: Eq a => [a] -> Eertree a
eertree = foldl (flip add) empty
