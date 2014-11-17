--
--  Nesting.agda - Nestings and their zippers
--
--  Eric Finster
--

{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Mtl
open import Tree

module Nesting where

  open Monad maybeM hiding (fmap ; η ; μ)

  data Nesting₀ (A : Set) : ℕ → Set where
    obj : (ob : A) → Nesting₀ A 0
    ext : {n : ℕ} → (a : A) → (cr : Tree n (Address n)) → Nesting₀ A (suc n)
    int : {n : ℕ} → (a : A) → (sh : Tree n (Nesting₀ A n)) → Nesting₀ A n
  
  Nesting : ℕ → Set → Set
  Nesting n A = Nesting₀ A n

  map-nesting : {n : ℕ} → {A B : Set} → (f : A → B) → Nesting n A → Nesting n B
  map-nesting {zero} f (obj ob) = obj (f ob)
  map-nesting {suc n} f (ext a corolla) = ext (f a) corolla
  map-nesting {n} f (int a shell) = int (f a) (fmap (map-nesting f) shell)
    where open Functor (TreeF n)

  traverse-nesting : {n : ℕ} → {A B : Set} → {G : Set → Set} → (isA : Applicative G) → (f : A → G B) → Nesting n A → G (Nesting n B)
  traverse-nesting {zero} isA f (obj ob) = let open Applicative isA in pure obj ⊛ f ob
  traverse-nesting {suc n} isA f (ext a corolla) = let open Applicative isA in pure ext ⊛ f a ⊛ pure corolla
  traverse-nesting {n} isA f (int a shell) = pure int ⊛ f a ⊛ traverse isA (λ p → traverse-nesting isA f p) shell
    where open Applicative isA
          open Traverse (TreeT n)

  NestingF : (n : ℕ) → Functor (Nesting n)
  NestingF n = record { fmap = map-nesting {n} }
  
  NestingT : (n : ℕ) → Traverse (Nesting n)
  NestingT n = record { isFunctor = NestingF n ; traverse = traverse-nesting {n} }

  nestingWithPrefix : {n : ℕ} → {A : Set} → Address (suc n) → Nesting n A → Nesting n (A × Address (suc n))
  nestingWithPrefix {zero} pref (obj a) = obj (a , pref)
  nestingWithPrefix {suc n} pref (ext a cr) = ext (a , pref) cr
  nestingWithPrefix {n} pref (int a sh) = 
    int (a , pref) (map-tree (λ { (nst , dir) → nestingWithPrefix (dir ∷ pref) nst }) (zipWithAddress sh)) 

  nestingWithAddr : {n : ℕ} → {A : Set} → Nesting n A → Nesting n (A × Address (suc n))
  nestingWithAddr pd = nestingWithPrefix [] pd
