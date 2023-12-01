#!/usr/bin/env bqn
Calibration ← +´ 10‿1⊸× ∘ (⊑¨) ∘ ⋈⟜⌽
GetDigits ← 10⊸>⊸/ -⟜'0'
ParseDigits ← {
    haystack ← ⟨"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"⟩ ∾ ⋈¨ '0'⊸+ ∘↕10

    10| (≠haystack)⊸>⊸/ (⌊´ haystack⊸⊐∘↑)¨ ↓ 𝕩
}

input ← •FLines "/dev/stdin"

•Show +´(Calibration∘GetDigits)¨ input
•Show +´(Calibration∘ParseDigits)¨ input

