{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (Char)
import Data.List (tails, find)
import Data.Maybe (maybeToList)
import Control.Arrow (first, second)


data Trie a = Trie [a] [(a, Trie a)]
    deriving Show

addPath :: Eq a => [a] -> Trie a -> Trie a
addPath = _

subpalindromes :: [a] -> [[a]]
subpalindromes = _

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
        Nothing -> t { maxSuff = [c], revPref = maxSuff t ++ revPref t }
        Just (pref, suff) -> t { maxSuff = c:suff ++ [c], revPref = tail pref } 

eertree :: Eq a => [a] -> Eertree a
eertree = foldl (flip add) empty
