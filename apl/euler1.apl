N ← 1000
⍝ First solution
+/ ((0=5|⍳(N-1)) ∨ (0=3|⍳(N-1))) / ⍳(N-1)
⍝ Simple solution
+ / (∨ ⌿ 0=3 5 ∘.|⍳(N-1)) / ⍳(N-1)
⍝ More general solution
+/ (3 × ⍳(⌊(N-1)÷3)) ∪ (5 × ⍳(⌊(N-1)÷5))
⍝ Dani solution
(+/ 3 × ⍳(⌊(N-1)÷3)) + (+/ 5 × ⍳(⌊(N-1)÷5)) - (+/ (3×5) × ⍳(⌊(N-1)÷(3×5)))
