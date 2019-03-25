{-# LANGUAGE ViewPatterns #-}

-- | A demonstration of an infinite EERTREE in a unary alphabet.

module Binary where

import           Data.List (transpose, nub, maximumBy)
import           Data.Ord  (comparing)

-- | A node of an infinite unary eertree.
data Node a = Node
  { len :: Int
  , value :: [a]   -- ^ Length of a palindrome at this node.
  , edge :: [(a, Node a)]  -- ^ Next palindromes.
  , link :: Node a -- ^ Max palindromic suffix.
  } deriving Show

data Eertree a = Eertree
    { maxPrefLink :: Node a
    , maxSuffLink :: Node a
    , pref        :: [a]
    , suff        :: [a]
    , pals        :: [Node a]
    }

alpha = "ab"

emptyNode :: Node Char
emptyNode = Node
  { len = 0
  , value = []
  , edge = []
  , link = emptyNode
  }

-- | Odd root (-1), parent of singleton palindromes.
oddNode :: Node Char
oddNode = Node
  { len = -1
  , value = ""
  , edge = nextOddNodes oddNode
  , link = oddNode
  }

-- | Even root (0), corresponds to empty palindrome.
evenNode :: Node Char
evenNode = Node
  { len = 0
  , value = ""
  , edge = nextNode evenNode
  , link = oddNode
  }


isPalindrome :: Eq a => [a] -> Bool 
isPalindrome s = s == reverse s

candidates :: [a] -> [[a]]
candidates [] = [[]]
candidates (c:suff) = (c:suff) : (candidates suff) 

maxSuff :: Eq a => [a] -> [a]
maxSuff [] = []
maxSuff (_:s) = head (filter isPalindrome (candidates s)) 

nodeByValue :: [Char] -> Node Char
nodeByValue suff
    | length suff `mod` 2 == 0 = nodeByPath half evenNode
    | otherwise                = nodeByPath half oddNode
    where
        half = drop (length suff `div` 2) suff

nodeByPath :: [Char] -> Node Char -> Node Char
nodeByPath [] start  = start
nodeByPath (c:s) start = nodeByPath s nextPoint 
    where
        nextPoint = snd (head (filter (\x -> fst x == c) (edge start)))

createNode :: Node Char -> Char -> (Char, Node Char)
createNode node c = (c, newNode)
    where
        newNode = Node
              { len = len node + 2 
              , value = c:(value node) ++ [c]
              , edge = nextNode newNode
              , link = nodeByValue (maxSuff(c:(value node) ++ [c]))             
              }

-- | Method to get to the next node.
nextNode :: Node Char -> [(Char, Node Char)]
nextNode parent = map (createNode parent) alpha


nextOddNodes :: Node Char -> [(Char, Node Char)]
nextOddNodes parent = map (createOddNode parent) alpha
    where 
        createOddNode node c = (c, newNode)
            where
                newNode = Node
                      { len = 1
                      , value = [c]
                      , edge = nextNode newNode
                      , link = evenNode             
                      }


-- | Collect all descendants of a node.
descendants :: Node Char -> [Node Char]
descendants node = desc ++ interleave (map descendants desc)
    where
        interleave = concat . transpose
        desc = map snd (edge node)


-- | Collect all nodes in BFS traversal.
-- Since we only have unary alphabet, this is trivial:
-- we get an infinite list of odd and an infinite list of even nodes.
-- We then interleave them and return.
--
-- Note: this starts with 'oddNode' which does not correspond
-- to a real palindrome.
allNodes :: [Node Char]
allNodes = merge (oddNode:(descendants oddNode)) (evenNode:(descendants evenNode))
  where
    merge (c1:s1) (c2:s2) 
        | length (value c1) > length (value c2) = [c2] ++ merge (c1:s1) s2
        | otherwise                             = [c1] ++ merge s1 (c2:s2)


-- | Extract palindrome that is represented by a given 'Node'.
--
-- >>> palindromeOf evenNode
-- ""
-- >>> palindromeOf (edge (edge oddNode))
-- "aaa"
--
-- NOTE: 'oddNode' does not correspond to a real palindrome,
-- but 'palindromeOf' will return empty sequence.
palindromeOf :: Node a -> [a]
palindromeOf node = value node

main :: IO ()
main = do
  let realNodes = drop 1 allNodes               -- remove oddNode
      palindromes = map palindromeOf realNodes  -- convert to palindromes
  mapM_ print (take 20 palindromes)             -- print first 10 palindromes



empty :: Eertree Char
empty = Eertree
    { maxPrefLink = evenNode
    , maxSuffLink = evenNode
    , pref        = ""
    , suff        = ""
    , pals        = []
    }

add :: Char -> Eertree Char -> Eertree Char
add c t = g $
    case pref t of
        _ | len (maxSuffLink t) == -1 -> t
            { maxSuffLink = nodeByValue [c]
            , pref        = pref t
            , suff        = suff t ++ [c]
            , pals        = pals t ++ [nodeByValue [c]]
            } 
        c':cs | c == c' -> t
            { maxSuffLink = newMaxSuff    
            , pref        = cs
            , suff        = suff t ++ [c]
            , pals        = pals t ++ [newMaxSuff]
            }
        _ -> add c t'
    where
            newMaxSuff = nodeByValue (c: value (maxSuffLink t) ++ [c])
            t' = t
                { maxSuffLink = link (maxSuffLink t)
                , pref        = drop (len (link (maxSuffLink t))) (value (maxSuffLink t)) ++ pref t 
                }
            g t
              | pref t == "" = t{suff = "", maxPrefLink = maxSuffLink t}
              | otherwise = t 

prepend :: Char -> Eertree Char -> Eertree Char
prepend c t = g $
    case suff t of
        _ | len (maxPrefLink t) == -1 -> t
            { maxPrefLink = nodeByValue [c]
            , pref        = pref t ++ [c]
            , suff        = suff t
            , pals        = pals t ++ [nodeByValue [c]]
            } 
        c':cs | c == c' -> t
            { maxPrefLink = newPrefSuff    
            , pref        = pref t ++ [c]
            , suff        = cs
            , pals        = pals t ++ [newPrefSuff]
            }
        _ -> prepend c t'
    where
            newPrefSuff = nodeByValue (c: value (maxPrefLink t) ++ [c])
            t' = t
                { maxPrefLink = link (maxPrefLink t)
                , suff        = drop (len (link (maxPrefLink t))) (value (maxPrefLink t)) ++ suff t 
                }
            g t
              | suff t == "" = t{pref = "", maxSuffLink = maxPrefLink t}
              | otherwise = t 


gen :: Int -> [String]
gen 0 = [[]]
gen n = concat
        [ ['a':s, 'b':s]
        | s <- gen (n - 1) ]

checkPrep :: Int -> Bool
checkPrep n = all (\x -> suff (eertree x) == suff (eertree' x)) (gen n)

eertree :: String -> Eertree Char
eertree = foldl (flip add) empty

eertree' :: String -> Eertree Char
eertree' s = foldl (flip prepend) empty (reverse s)

palindromesOf :: String -> [String]
palindromesOf = nub . map value . pals . eertree

mergeEertees :: Eertree Char -> Eertree Char -> Eertree Char
mergeEertees t1 t2 = Eertree 
            { maxPrefLink = newPrefLink            
            , maxSuffLink = newSuffLink 
            , pref        = reverse (take ((length (fstStr ++ sndStr)) - len newSuffLink) (fstStr ++ sndStr))
            , suff        = drop (len newPrefLink) (fstStr ++ sndStr)
            , pals        = newPals ++ pals t1 ++ pals t2
            }
    where
        newPrefLink  = nodeByValue (maximumBy (comparing length) wholePref) -- ^ taking best from candidates
        newSuffLink  = nodeByValue (maximumBy (comparing length) wholeSuff) -- ^ taking best from candidates
        wholePref    = map (\(_,x,_) -> x) (filter (\(x,_,_) -> x == "") foundPals) ++ [value (maxPrefLink t1)] -- ^ candidates for being maxPref
        wholeSuff    = map (\(_,x,_) -> x) (filter (\(_,_,x) -> x == "") foundPals) ++ [value (maxSuffLink t2)] -- ^ candidates for being maxSuff
        foundPals    = rightPals ++ leftPals ++ middlePals             -- ^ all information about new found palinfromes
        newPals      = map nodeByValue (map (\(_,x,_) -> x) foundPals) -- ^ adding to pals list (which is list of Nodes)
        middlePals   = (palsFromMiddle (reverse fstStr) sndStr "")     -- ^ searching for palinfromes from (joint) middle evenly
        leftPals     = (concat (zipWith3 palsFromMiddle fstLeftPals sndLeftPals midLeftPals)) 
                -- ^ searching for palinfromes near suffix of first str
        sndLeftPals  = replicate (length midLeftPals) sndStr
        fstLeftPals  = map (\x -> reverse(take ((length fstStr) - (length x)) fstStr)) midLeftPals
        midLeftPals  = map value (collectSuffixes (maxSuffLink t1))
                -- ^ creating lists of different inputs depending on palinfrome center
        rightPals    = (concat (zipWith3 palsFromMiddle fstRightPals sndRightPals midRightPals))
                -- ^ searching for palinfromes near prefix of second str
        fstRightPals = replicate (length midRightPals) (reverse fstStr)
        sndRightPals = map (\x -> drop (length x) sndStr) midRightPals
        midRightPals = map value (collectSuffixes (maxPrefLink t2))
                -- ^ creating lists of different inputs depending on palinfrome center
        fstStr       = reverse (pref t1) ++ value (maxSuffLink t1) -- ^ string of first tree
        sndStr       = reverse (pref t2) ++ value (maxSuffLink t2) -- ^ string of second tree

-- | Creates palindrome with the middle of value i
-- | Contain information about prefix before palinfrome and suffix after
-- >>> palsFromMiddle "aba" "aba" ""
-- [("ba","aa","ba"),("a","baab","a"),("","abaaba","")]
palsFromMiddle :: String -> String -> String -> [(String, String, String)]
palsFromMiddle _ [] _ = []
palsFromMiddle [] _ _ = []
palsFromMiddle (c:s) (c':s') i
    | c == c'   = (s,newOne,s'):palsFromMiddle s s' newOne
    | otherwise = []
    where
        newOne = c:i ++ [c]

collectSuffixes :: Node Char -> [Node Char]
collectSuffixes a
    | len a == 0 = []
    | otherwise  = a: collectSuffixes (link a)


checkMerge :: String -> String -> [String]
checkMerge s s' = map value (pals (mergeEertees (eertree s) (eertree s')))

a216264 :: Int -> Int
a216264 n = length strings
    where
        strings = [ s
                  | s <- gen n 
                  , length (palindromesOf s) == n]

