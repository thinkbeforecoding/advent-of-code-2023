&fras "./input/day01.txt"
⊕□ ⍜▽¯ ⊃⌕ (\+⌕) @\n .▽≠ @\r . # remove \r and split on \n
.
# Part1
🔢 ← +@1⇡9
👈 ← -@0 ⊡ /↧ ⊗🔢 .
👉 ← 👈⇌
/+ ≡(+ ×10 ⊃👈👉 ⊔)

# Part2
:
🔤 ← ⊂ ≡□ ↯ 9_1 +@1⇡9 {"one" "two" "three" "four" "five" "six" "seven" "eight" "nine"}
🤛 ← ;: +1◿9 ⊗ /↧ . ≡(⊔ ⍜⊔ (⊗ 1 ⌕⊙.)) 🔤
🤜 ← ;: +1◿9 ⊗ /↧ . ≡(⊔ ⍜⊔ (⊗ 1 ⌕⊙.)) ≡(⍜⊔ ⇌) 🔤 ⇌
/+ ≡(+ ×10 ⊃🤛🤜 ⊔)
