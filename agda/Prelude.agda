--
--  Prelude.agda - Basic definitions
--
--  Eric Finster
--

module Prelude where

  infixr 5 _∨_ _∷_ _++_
  infixr 1 _⊎_
  infixr 4 _,_
  infixr 2 _×_

  data ℕ : Set where
    zero : ℕ
    suc  : (n : ℕ) → ℕ

  {-# BUILTIN NATURAL ℕ #-}

  record ⊤ : Set where
    constructor tt

  data Bool : Set where
    true  : Bool
    false : Bool

  {-# BUILTIN BOOL  Bool  #-}
  {-# BUILTIN TRUE  true  #-}
  {-# BUILTIN FALSE false #-}

  {-# COMPILED_DATA Bool Bool True False #-}

  _∨_ : Bool → Bool → Bool
  true  ∨ b = true
  false ∨ b = b

  data Maybe (A : Set) : Set where
    just    : (x : A) → Maybe A
    nothing : Maybe A

  {-# IMPORT Data.FFI #-}
  {-# COMPILED_DATA Maybe Data.FFI.AgdaMaybe Just Nothing #-}

  data _⊎_ (A : Set) (B : Set) : Set where
    inj₁ : (x : A) → A ⊎ B
    inj₂ : (y : B) → A ⊎ B

  {-# IMPORT Data.FFI #-}
  {-# COMPILED_DATA _⊎_ Data.FFI.AgdaEither Left Right #-}

  postulate
    String : Set

  {-# BUILTIN STRING String #-}
  {-# COMPILED_TYPE String String #-}

  record Σ (A : Set) (B : A → Set) : Set where
    constructor _,_
    field
      proj₁ : A
      proj₂ : B proj₁

  open Σ public

  Σ-syntax : ∀ (A : Set) → (A → Set) → Set 
  Σ-syntax = Σ

  syntax Σ-syntax A (λ x → B) = Σ[ x ∈ A ] B

  _×_ : ∀ (A : Set) (B : Set) → Set
  A × B = Σ[ x ∈ A ] B

  data List (A : Set) : Set where
    []  : List A
    _∷_ : (x : A) (xs : List A) → List A

  {-# BUILTIN LIST List #-}
  {-# BUILTIN NIL  []   #-}
  {-# BUILTIN CONS _∷_  #-}

  {-# IMPORT Data.FFI #-}
  {-# COMPILED_DATA List Data.FFI.AgdaList [] (:) #-}

  [_] : ∀ {A : Set} → A → List A
  [ x ] = x ∷ []

  _++_ : ∀ {A : Set} → List A → List A → List A
  []       ++ ys = ys
  (x ∷ xs) ++ ys = x ∷ (xs ++ ys)

  data ⊥ : Set where

  {-# IMPORT Data.FFI #-}
  {-# COMPILED_DATA ⊥ Data.FFI.AgdaEmpty #-}
