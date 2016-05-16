Edit Distance
=====================

This is a small script for calculating the edit distance (Levenshtein distance) 
between two list of almost arbitrary elements. It is a generic implementation 
of the regular _O(n*m)_ algorithm that doesn't do any fancy optimalizations (e.g.
when the strings are the same). On the other hand it accepts custom costs functions
for insertions, deletions, and subsitutions.

Usage
-----

There're two ways to use this script. If you're fine with the regular cost of one for
one element insertion, deletion, and substitution then all you need to do is to import
`EditDistance` and call `editDistanceDef` with two lists as arguments. The function
returns a regular `Maybe Int` as the final minimal cost (Nothing means that you can't
transform the original list to the desired one using specified modification functions). 

You can also create your own scoring functions. For insertion and deletion the type is 
`data FUn a = FUn Int ([a] -> Maybe Int)` where the first `Int` argument
specifies the maximum length of a sub-list that can be processed by the function at once
(i.e. inserted / removed). For substitution the type is `FBin (Int, Int) ([a] -> [a] -> Maybe Int)`
where the touple specifies the maximum length of a sub-list from original list that can be replaced 
and the maximum length of a sub-list from desired list that the one from original can be replaced with.


````{haskell}
import           EditDistance

editDistanceDef "a" "abc"
editDistanceDef [1] [1,2,3]

advEDSetup = editDistance frem fadd fbin
   where 
      frem = FUn 3 (\x -> if x == [head x | a <- [1..(length x)]] then Just 1 else Just (length x))
      fadd = FUn 3 (\x -> if x == [head x | a <- [1..(length x)]] then Just 1 else Just (length x))
      fbin = FBin (4,4) (\x y -> if x == y then Just 0 else (if x == "abcd" && y == "cdfe" then Just 1 else Just (length x + length y)))

advEDSetup "aabcdd" "acdfed"
advEDSetup "abb" "a"
advEDSetup "abc" "a"

````

Tests
-----
There're also two sets of 'tests' in `Tests.hs`. They can be run by calling `advTests` and `simpleTests` respectively.

Disclaimer
-----
This script was written as a final project for a semester long course "Non-procedural programming" at [MFF CUNI](http://www.mff.cuni.cz/) 
and is therefore neither the-best optimized nor generally the best implementation of edit distance algorithm in haskell there is. Use 
at own risk.

Licence
-----
[MIT](https://opensource.org/licenses/MIT)