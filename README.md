# Countdown Solver
Solvers for the popular British television program [Countdown](https://en.wikipedia.org/wiki/Countdown_(game_show)).

## Numbers Game

The solver relies on a "non-deterministic computations" module, which
provides a function that takes as input a list of elements and a list of
partial operators, and continuously applies the operators on the
elements in all possible ways. This yields a list of resulting elements
with a computation history attached to each.

The solver then invokes this function with all choices of the given
input numbers, and filters out the results whose value does not match
the target.

### Example

On input `[100; 8; 1; 8; 2; 10]` with target `719`, the solver returns
`1392` solutions (Runtime: ~10 seconds). Below are two of the `1392` listed.
```
100 - 10 = 90
8 * 90 = 720
720 - 1 = 719

---------------------

...

---------------------

100 * 8 = 800
800 - 2 = 798
10 * 8 = 80
80 - 1 = 79
798 - 79 = 719

---------------------
...
```

### Archived Solvers

The Archived library contains naive solvers to the numbers game which
sacrifice completeness for efficiency. That is, these solvers might miss
a solution.

## Letters Game

The letters game solver works by representing words as "frequency maps"
whose keys are characters and values are numbers. A key-value pair
`(k,v)` in a frequency map for some word represents that the character
`k` appears `v` times in the word. Frequency maps ignore the order of
the characters in a word. Hence, the frequency map for a word simply
encodes how many of each character is required to build the word.

The solver starts by building the frequency map for the input letters.
Next, it filters the list of all words by computing the frequency map
for each word, and checking if the frequency map is a "subset" of the
frequency map of the input letters. If it is, then we can build the word
using the input letters.

As such, after filtering, we are left with only those words we can build
from the input letters. In the final step, the remaining words are sorted by length.

### Example

On input `['s'; 't'; 'p'; 'b'; 'u'; 'a'; 'e'; 'r'; 's']` the solver
returns the following list (shortened to 10 words):
```
abstruse
pastures
abusers
breasts
busters
pasture
abrupt
abuses
assert
assure
```
