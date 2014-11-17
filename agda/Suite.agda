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
    ■ : (bs : T 0) → Suite T 0
    _⟫_ : {n : ℕ} → (tl : Suite T n) → (hd : T (suc n)) → Suite T (suc n)

  head : {n : ℕ} → {T : ℕ → Set} → Suite T n → T n
  head {zero} (■ x) = x
  head {suc n} (σ ⟫ x) = x

  tail : {n : ℕ} → {T : ℕ → Set} → Suite T (suc n) → Suite T n
  tail (σ ⟫ x) = σ
