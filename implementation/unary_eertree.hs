-- | A demonstration of an infinite EERTREE in a unary alphabet.
module Unary where

import           Data.List (transpose)

-- | A node of an infinite unary eertree.
data Node = Node
  { len  :: Int   -- ^ Length of a palindrome at this node.
  , edge :: Node  -- ^ Next palindrome.
  , link :: Node  -- ^ Max palindromic suffix.
  } deriving Show

-- | Odd root (-1), parent of singleton palindromes.
oddNode :: Node
oddNode = Node
  { len = -1
  , edge = nextNode oddNode
  , link = oddNode
  }

-- | Even root (0), corresponds to empty palindrome.
evenNode :: Node
evenNode = Node
  { len = 0
  , edge = nextNode evenNode
  , link = oddNode
  }

-- | Method to get to the next node.
nextNode :: Node -> Node
nextNode parent = node
  where
    node = Node
      { len = len parent + 2              -- 2 more letters added
      , edge = nextNode node              -- lazily compute next node
      , link = if len parent < 0
                  then evenNode           -- a singleton has empty suffix
                  else edge (link parent) -- otherwise follow parent link
      }

-- | Collect all descendants of a node.
-- Since we only have unary alphabet, this is trivial:
-- all descendants form a list.
descendants :: Node -> [Node]
descendants node = node : descendants (edge node)

-- | Collect all nodes in BFS traversal.
-- Since we only have unary alphabet, this is trivial:
-- we get an infinite list of odd and an infinite list of even nodes.
-- We then interleave them and return.
--
-- Note: this starts with 'oddNode' which does not correspond
-- to a real palindrome.
allNodes :: [Node]
allNodes = interleave [descendants oddNode, descendants evenNode]
  where
    -- NOTE: in more complex alphabets you might want
    -- to use merge (as in mergesort) to get a nice sequence of
    -- palindromes sorted by length and lexicographically.
    interleave = concat . transpose

-- | Extract palindrome that is represented by a given 'Node'.
--
-- >>> palindromeOf evenNode
-- ""
-- >>> palindromeOf (edge (edge oddNode))
-- "aaa"
--
-- NOTE: 'oddNode' does not correspond to a real palindrome,
-- but 'palindromeOf' will return empty sequence.
palindromeOf :: Node -> String
palindromeOf node = replicate (len node) 'a'

main :: IO ()
main = do
  let realNodes = drop 1 allNodes               -- remove oddNode
      palindromes = map palindromeOf realNodes  -- convert to palindromes
  mapM_ print (take 10 palindromes)             -- print first 10 palindromes
