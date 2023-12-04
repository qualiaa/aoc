#!/usr/bin/env bqn
StrToInt ← +⟜(10⊸×)´ ∘ ⌽ ∘ -⟜'0'

# From filters 𝕩 to remove elements not in 𝕨 (i.e. take 𝕨 From 𝕩)
From ← { ∊⟜𝕨⊸/ 𝕩 }

# Use these for extracting alpha and numeric characters
alpha ← 'a' +↕26
numeric ← '0' + ↕10

# Use these to group counts by colour and check part 1 condition
colours ← "red"‿"green"‿"blue"
limits ← 12‿13‿14

# Remove game numbers
input ← 8⊸↓¨ •FLines "/dev/stdin"
# Split into fields delimited by , and ;
fields ← { (((+` +´¨) ∘ (','‿';'⊸=⌜))¨ 𝕩) ⊔¨ 𝕩} input
# Extract alphabetic and numeric characters from fields
sequences ← {⟨StrToInt numeric From 𝕩, alpha From 𝕩⟩}¨¨ fields
#sequence ← ⟨ 3‿"blue", 4‿"red", 1‿"red", 2‿"green", 6‿"blue", 2‿"green" ⟩
sequence_limits ← {⌈´¨(colours⊐ 1⊸⊑¨ 𝕩) ⊔ ⊑¨ 𝕩}¨ sequences

•Show +´ 1+ / (∧´ limits⊸≥)¨ sequence_limits

•Show +´ ×´¨ sequence_limits
