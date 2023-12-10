#!/usr/bin/env bqn

StrToInt ← +⟜(10⊸×)´ ∘ ⌽ ∘ -⟜'0'
numeric ← '0' + ↕10

ParseNumberList ← {
  StrToInt¨ 0⊸<⟜(≠¨)⊸/ 1↓(1⊸(+`)∘¬⊸× ∊⟜numeric)⊸⊔ 𝕩
}

input ←  •FLines "/dev/stdin"

Solve ← {t‿r: 1+ t (⌈∘(-⟜1)∘+-⌊∘(1⊸+)∘-)○(÷⟜2) √((×˜t) - (4×r))}

•Show ×´ Solve ParseNumberList¨ input
•Show Solve StrToInt ∘ (∊⟜numeric⊸/)¨ input
