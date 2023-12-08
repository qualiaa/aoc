#!/usr/bin/env bqn

StrToInt ← +⟜(10⊸×)´ ∘ ⌽ ∘ -⟜'0'
numeric ← '0' + ↕10

input ← •FLines "/dev/stdin"

# FIXME: Feel like this parsing could be less terrible
sides ← (1⊸↓) ∘ ((+`) ∘ (∨´¨) ∘ ('|'‿':'⊸=⌜)⊸⊔)¨ input
digits ←  (1⊸↓) ∘ ((» ∘ (¯1⊸(+`) ∘ ¬ ∘ -⟜») ∘ (+`)⊸× ∘ ∊⟜numeric)⊸⊔)¨¨ sides

array ← > StrToInt¨ ∘ ((1⊸≤∘≠¨)⊸/)¨¨ digits
left‿right ← ((1‿∘⊸⥊)⊸∾)¨˝ array

•Show +´ left ((2⊸⋆×≥⟜0) ∘ -⟜1 ∘ (+˝) ∘ ∊)˘ right

