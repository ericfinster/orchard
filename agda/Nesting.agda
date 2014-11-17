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
    obj : (a : A) → Nesting₀ A 0
    ext : {n : ℕ} → (a : A) → (cr : Tree n (Address n)) → Nesting₀ A (suc n)
    int : {n : ℕ} → (a : A) → (sh : Tree n (Nesting₀ A n)) → Nesting₀ A n
  
  Nesting : ℕ → Set → Set
  Nesting n A = Nesting₀ A n

  map-nesting : {n : ℕ} → {A B : Set} → (f : A → B) → Nesting n A → Nesting n B
  map-nesting {zero} f (obj a) = obj (f a)
  map-nesting {suc n} f (ext a corolla) = ext (f a) corolla
  map-nesting {n} f (int a shell) = int (f a) (fmap (map-nesting f) shell)
    where open Functor (TreeF n)

  traverse-nesting : {n : ℕ} → {A B : Set} → {G : Set → Set} → (isA : Applicative G) → (f : A → G B) → Nesting n A → G (Nesting n B)
  traverse-nesting {zero} isA f (obj a) = let open Applicative isA in pure obj ⊛ f a
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

  base-value : {n : ℕ} → {A : Set} → Nesting n A → A
  base-value (obj a) = a
  base-value (ext a cr) = a
  base-value (int a sh) = a

  spine : {n : ℕ} → {A : Set} → Nesting n A → Maybe (Tree n A)
  spine {zero} (obj a) = just (pt a)
  spine {suc n} (ext a cr) = just (node a (map-tree leaf cr))
  spine {n} (int a sh) = traverse-tree maybeA spine sh >>= join

  mutual

    DerivativeNst : (n : ℕ) → Set → Set
    DerivativeNst n A = Tree n (Nesting n A) × ContextNst n A

    ContextNst : (n : ℕ) → Set → Set
    ContextNst n A = List (A × Derivative n (Nesting n A))

    ZipperNst : (n : ℕ) → Set → Set
    ZipperNst n A = Nesting n A × ContextNst n A

    plug : {n : ℕ} → {A : Set} → DerivativeNst n A → A → Nesting n A
    plug (t , c) a = close c (int a t)

    close : {n : ℕ} → {A : Set} → ContextNst n A → Nesting n A → Nesting n A
    close [] nst = nst
    close ((a , d) ∷ ds) nst = close ds (int a (d ← nst))

    visit-nesting : {n : ℕ} → {A : Set} → Address n → ZipperNst n A → Maybe (ZipperNst n A)
    visit-nesting addr (obj a , cntxt) = nothing
    visit-nesting addr (ext a corolla , cntxt) = nothing
    visit-nesting {zero} [] (int h (pt t) , cntxt) = just (t , (h , tt) ∷ cntxt)
    visit-nesting {zero} (() ∷ addr) (int a shell , cntxt)
    visit-nesting {suc n} addr (int a shell , cntxt) = 
      seek addr (shell , []) 
      >>= (λ { (leaf l , c) → nothing ; 
               (node pd hsh , c) → just (pd , ((a , (hsh , c)) ∷ cntxt)) 
             })

    seek-nesting : {n : ℕ} → {A : Set} → Address (suc n) → ZipperNst n A → Maybe (ZipperNst n A)
    seek-nesting [] z = just z
    seek-nesting (d ∷ ds) z = seek-nesting ds z >>= visit-nesting d

    sibling : {n : ℕ} → {A : Set} → Address n → ZipperNst (suc n) A → Maybe (ZipperNst (suc n) A)
    sibling dir (fcs , []) = nothing
    sibling {zero} dir (fcs , (a , pt (leaf l) , hcn) ∷ cn) = nothing
    sibling {zero} dir (fcs , (a , pt (node nfcs shell) , hcn) ∷ cn) = 
      just (nfcs , ((a , (shell , ((fcs , tt) ∷ hcn))) ∷ cn))
    sibling {suc n} dir (fcs , (a , verts , hcn) ∷ cn) = 
      seek dir (verts , []) 
      >>= (λ { (leaf l , vcn) → nothing ; 
               (node (leaf l) shell , vcn) → nothing ; 
               (node (node nfcs vrem) hmask , vcn) → 
                 just (nfcs , (a , (vrem , (fcs , (hmask , vcn)) ∷ hcn)) ∷ cn) })



  
