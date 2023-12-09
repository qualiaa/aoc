#!/usr/bin/env bqn

StrToInt ← +⟜(10⊸×)´ ∘ ⌽ ∘ -⟜'0'
numeric ← '0' + ↕10

input ← •FLines "/dev/stdin"

# FIXME: Feel like this parsing could be less terrible
sides ← 1⊸↓ ∘ ((+`) ∘ (∨´¨) ∘ ('|'‿':'⊸=⌜)⊸⊔)¨ input
digits ←  1⊸↓ ∘ ((» ∘ (¯1⊸(+`) ∘ ¬ ∘ -⟜») ∘ (+`)⊸× ∘ ∊⟜numeric)⊸⊔)¨¨ sides

drawn‿winners ← ⟨1,∘⟩⊸⥊ ⊸∾¨˝ > StrToInt¨ ∘ ((1⊸≤∘≠¨)⊸/)¨¨ digits

win_counts ← drawn +˝∘∊˘ winners

•Show +´ (2⊸⋆×≥⟜0)¨ win_counts - 1

R ← {
  total‿future_counts ← 𝕩
  n ← ⊑ future_counts
  ⟨total + n, ≠∘⊢ ⊸↑⟜(𝕨 ⥊ n)⊸+ 1↓future_counts⟩
}

•Show ⊑ 0‿(⥊⟜1 ≠drawn) R´ ⌽ win_counts

