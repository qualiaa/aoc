#!/usr/bin/env bqn
input ← •FLines "/dev/stdin"

Calibration ← +´ 10‿1⊸× ∘ (⊑¨) ∘ ⋈⟜⌽
GetDigits ← 10⊸>⊸/ -⟜'0'

ParseDigits ← {
    haystack ← ⟨"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"⟩ ∾ ⋈¨ '0'⊸+ ∘↕10
    10| (≠haystack)⊸>⊸/ (⌊´ haystack⊸⊐∘↑)¨ ↓ 𝕩
}

#eg ← "ddgjgcrssevensix37twooneightgt"
#
#•Out "Example: " ∾ •Fmt eg
#•Out "Result: " ∾ •Fmt ParseDigits eg

#•Out "Result: " ∾ •Fmt ParseDigits "five1nine"

•Show +´(Calibration∘GetDigits)¨ input
•Show +´(Calibration∘ParseDigits)¨ input

