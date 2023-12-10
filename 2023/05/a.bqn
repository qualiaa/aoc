#!/usr/bin/env bqn

StrToInt ← +⟜(10⊸×)´ ∘ ⌽ ∘ -⟜'0'
numeric ← '0' + ↕10

ParseNumberList ← {
  StrToInt¨ 0⊸<⟜(≠¨)⊸/ 1↓(1⊸(+`)∘¬⊸× ∊⟜numeric)⊸⊔ 𝕩
}

ParseGroup ← {
  lines ← 1 ↓ 𝕩  # Drop label line
  n ← ⊑ ⊐⟜⟨⟨⟩⟩ lines
  ⟨∧ (1‿0‿2⊸⊏˘) > ParseNumberList¨ n↑lines ,  (n+1) ↓lines⟩
}

ParseGroups ← {g‿⟨⟩: ⋈g; g‿x: (⋈g) ∾ ParseGroups x} ∘ ParseGroup

Lookup ← {
  LookupOne ← {
    src‿dest‿size ← 𝕩
    (0⊸≤∧<⟜size)∘(-⟜src)◶⟨⊢, dest⊸+ -⟜src⟩ ⊑ 𝕨
  }
  𝕨 LookupOne˘  (0⌈˘ ((⊏˘ 𝕩) ⍋ 𝕨) - 1) ⊏ 𝕩
}

input ← •FLines "/dev/stdin"

seeds ← ParseNumberList ⊑ input
maps ← ParseGroups 2↓ input

•Show ⌊´ seeds Lookup˜´ (⌽maps)
# Part B solution is to compose the maps into one and identify crossing
# points... Not sure if I can be bothered.


