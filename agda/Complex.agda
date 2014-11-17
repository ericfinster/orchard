--
--  Complex.agda - Complexes
--

open import Prelude
open import Mtl
open import Tree
open import Nesting
open import Suite

module Complex where

  open Monad maybeM hiding (fmap ; η ; μ)

  Complex : (ℕ → Set) → ℕ → Set
  Complex A n = Suite (λ n → Nesting n (A n)) n

  sourceAt : {n : ℕ} → {A : ℕ → Set} → Complex A n → Address (suc n) → Maybe (Complex A n)
  sourceAt = {!!}

  comult : {n : ℕ} → {A : ℕ → Set} → Complex A n → Maybe (Complex (Complex A) n)
  comult {zero} (■ (obj ob)) = just (■ (obj (■ (obj ob))))
  comult {zero} (■ (int a (Pt nst))) = 
    comult (■ nst) >>= (λ { (■ res) → just (■ (int (■ (obj a)) (Pt res))) })
  comult {suc n} (ic ⟫ nst) = 
    traverse-nesting maybeA (λ { (_ , addr) → sourceAt (ic ⟫ nst) addr }) (nestingWithAddr nst) 
    >>= (λ hd → comult ic 
    >>= (λ tl → just (tl ⟫ hd)))
