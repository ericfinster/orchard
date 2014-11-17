--
--  Suite.agda - Indexed sequences
--
--  Eric Finster
--

open import Prelude
open import Mtl
open import Nesting

module Suite where

  data Suite (T : ℕ → Set) : ℕ → Set where
    ■ : T 0 → Suite T 0
    _⟫_ : {n : ℕ} → Suite T n → T (suc n) → Suite T (suc n)

