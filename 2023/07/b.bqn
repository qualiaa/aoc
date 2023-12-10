#!/usr/bin/env bqn

input ← •FLines "/dev/stdin"

card_rank ← ⌽"AKQT98765432J"

StrToInt ← +⟜(10⊸×)´ ∘ ⌽ ∘ -⟜'0'
OldClassify ← {(-⟜1∘≠¨ ⍷⊸⊐⊸⊔ 𝕩)⊸(+´∘=)¨ ⌽↕5}
NewClassify ← {
nonjack‿jack ← 'J'⊸=⊸⊔ 𝕩
{ 5= ≠jack ? 1»5⥊0;
old ← OldClassify nonjack
# FIXME: I'm sure there's a better way of doing this
from ← ⊑∘(0⊸<⊸⊐⟜1) old
to ← from - ≠ jack
(+⟜1)⌾(to⊸⊏)(-⟜1)⌾(from⊸⊏) old
}
}
Classify ← {⊑ 'J'∊𝕩 ? NewClassify 𝕩 ; OldClassify 𝕩}
SortKey ← {(Classify ⋈card_rank⊸⊐) 𝕩}

arr ← StrToInt¨⌾(1⊸⊏˘) > 1⊸↓ ∘ ((1⊸(+`)⊸×⟜¬ ∘ ∊⟜" ")⊸⊔)¨ input
•Show +´ (1+ ⍋⍋ SortKey∘⊑˘ arr) × (1⊸⊏˘ arr)
