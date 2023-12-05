#!/usr/bin/env bqn

StrToInt ← +⟜(10⊸×)´ ∘ ⌽ ∘ -⟜'0'
IsInt ← ∞⊸>
numeric ← '0' + ↕10

input ← >•FLines "/dev/stdin"

# Get indices of all digits within flat array
digit_indices ← / ⥊input ∊ numeric

# Calculate indices of numbers by appearance
# FIXME: Feel like there's a neater way of handling the ¯1+´ ... 1≠ here
number_indices ← ¯1+` 1≠ -⟜» digit_indices

# Replace digit characters with the index of that number
fixed ← number_indices˙ ⌾(digit_indices⊸⊏ ∘ ⥊) input

# Get windows around special characters (i.e. not '.' and not integer)
# FIXME: Repetition of ∘‿3‿3 seems wrong. Feels like I should be able to
#        collapse the first axes, but can't see how to do it.
# FIXME: I think I could simplify this solution by just grabbing all the indices
#        in each major cell here.
symbol_neighbours ← (('.'⊸≠)⊸∧⟜(¬∘IsInt) ∘ (1‿1⊸⊑˘))⊸/ ∘‿3‿3 ⥊ 3‿3↕fixed

# Take the unique indices from each symbol neighbourhood
# FIXME: Again, feels like I should be able to remove an axis without these
#        shenanigans
# FIXME: ⥊ ⋈˘ seems like an ok way to convert an array to a list of its major
#        cells, but don't know if idiomatic or just idiotic
part_indices ← ⍷∘(IsInt⊸/)¨ ⥊ ⋈˘ ∘‿9⥊ symbol_neighbours

# Group the digit characters and parse to get list of numbers
numbers ← StrToInt∘(⊢⊸⊏⟜(⥊input))¨ (number_indices ⊔ digit_indices)

# Convert indices into part numbers and sum
# FIXME: Not sure if this is the best away (or even a good way) to flatten
#        one(or all) levels of nesting from a list
•Show +´ (∾´ part_indices) ⊏ numbers

•Show +´ ×´¨ ⊏⟜numbers¨ (2⊸=∘≠¨)⊸/ ⍷∘(IsInt⊸/)∘⥊¨ ('*'= (1‿1⊸⊑)¨)⊸/ ⥊ ⋈˘ symbol_neighbours
