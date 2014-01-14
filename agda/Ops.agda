module Ops where

open import Data.Nat
open import Data.List
open import Relation.Binary.PropositionalEquality

mutual
  data Cell : ℕ → Set where
    obj : Cell 0
    composite : ∀{n} → CellTree n → Cell (suc n)

  data CellTree : ℕ → Set where
    singleton : CellTree 0
    leaf : ∀{n} → Cell n → CellTree (suc n)
    branch : ∀{n} → (c : Cell (suc n)) → (l : List (CellTree (suc n))) → (compatible c l) → CellTree (suc n)

  innerNodes : ∀{n} → CellTree n → List (Cell n)
  innerNodes singleton = obj ∷ []
  innerNodes (leaf x) = []
  innerNodes (branch c cs _) = c ∷ concat (map innerNodes cs)

  corolla : ∀{n} → Cell n → CellTree n
  corolla obj = singleton
  corolla (composite x) = branch (composite x) (map leaf (innerNodes x))

  -- 
  flatten : ∀{n} → CellTree (suc n) → CellTree n
  flatten (leaf x) = corolla x
  flatten (branch c cs) = {!!}

  -- This needs fixing!!
  target : ∀{n} → Cell (suc n) → Cell n
  target {zero} c = obj
  target {suc n} (composite x) = composite (flatten x)

  output : ∀{n} → CellTree (suc n) → Cell n
  output (leaf x) = x
  output (branch x x₁) = target x

  compatible : ∀ {n} → Cell (suc n) → List (CellTree (suc n)) → Set
  compatible (composite x) l = innerNodes x ≡ map output l
