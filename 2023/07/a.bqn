#!/usr/bin/env bqn

input ← •FLines "/dev/stdin"

card_rank ← ⌽"AKQJT98765432"

StrToInt ← +⟜(10⊸×)´ ∘ ⌽ ∘ -⟜'0'

arr ← StrToInt¨⌾(1⊸⊏˘) > 1⊸↓ ∘ ((1⊸(+`)⊸×⟜¬ ∘ ∊⟜" ")⊸⊔)¨ input

SortKey ← {((⊒ 𝕩)⊸(+´∘=)¨ (⌽↕5)) ⋈ card_rank⊸⊐ 𝕩}

•Show +´ (1+ ⍋⍋ SortKey∘⊑˘ arr) × (1⊸⊏˘ arr)
