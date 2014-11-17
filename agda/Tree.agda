--
--  Tree.agda - Higher Dimensional Trees
--
--  Eric Finster
--

{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Mtl

module Tree where

  mutual

    Direction : (n : ℕ) → Set
    Direction zero = ⊥
    Direction (suc n) = Address n

    Address : (n : ℕ) → Set
    Address n = List (Direction n)

  data Tree₀ (A : Set) : ℕ → Set where
    Pt : (a : A) → Tree₀ A 0
    Leaf : {n : ℕ} → (addr : Address n) → Tree₀ A (suc n)
    Node : {n : ℕ} → (a : A) → (sh : Tree₀ (Tree₀ A (suc n)) n) → Tree₀ A (suc n)

  Tree : ℕ → Set → Set
  Tree n A = Tree₀ A n

  map-tree : {n : ℕ} → {A B : Set} → (A → B) → Tree n A → Tree n B
  map-tree {zero} f (Pt a) = Pt (f a)
  map-tree {suc n} f (Leaf addr) = Leaf addr
  map-tree {suc n} f (Node a sh) = Node (f a) (map-tree (map-tree f) sh)

  traverse-tree : {n : ℕ} → {A B : Set} → {G : Set → Set} → Applicative G → (A → G B) → Tree n A → G (Tree n B)
  traverse-tree {zero} apG f (Pt a) = let open Applicative apG in pure Pt ⊛ f a
  traverse-tree {suc n} apG f (Leaf addr) = let open Applicative apG in pure (Leaf addr)
  traverse-tree {suc n} apG f (Node a sh) = let open Applicative apG in 
    pure Node ⊛ f a ⊛ traverse-tree apG (traverse-tree apG f) sh

  TreeF : (n : ℕ) → Functor (Tree n)
  TreeF n = record { fmap = map-tree {n} }

  TreeT : (n : ℕ) → Traverse (Tree n)
  TreeT n = record { isFunctor = TreeF n ; traverse = traverse-tree {n} }

  open Monad maybeM hiding (fmap)

  mutual

    Derivative : (n : ℕ) → Set → Set
    Derivative zero A = ⊤
    Derivative (suc n) A = Tree n (Tree (suc n) A) × Context (suc n) A

    Context : (n : ℕ) → Set → Set
    Context zero A = ⊤
    Context (suc n) A = List (A × Derivative n (Tree (suc n) A))

    Zipper : (n : ℕ) → Set → Set
    Zipper n A = Tree n A × Context n A
  
    _←_ : {n : ℕ} → {A : Set} → Derivative n A → A → Tree n A
    _←_ {zero} tt a = Pt a
    _←_ {suc n} (sh , context) a = context ↓ Node a sh

    _↓_ : {n : ℕ} → {A : Set} → Context n A → Tree n A → Tree n A
    _↓_ {zero} tt t = t
    _↓_ {suc n} [] t = t
    _↓_ {suc n} ((a , d) ∷ c) t = c ↓ Node a (d ← t)

    visit : {n : ℕ} → {A : Set} → Direction n → Zipper n A → Maybe (Zipper n A)
    visit {zero} () z
    visit {suc zero} [] (Leaf addr , c) = nothing
    visit {suc zero} [] (Node head (Pt tail) , c) = just (tail , (head , tt) ∷ c)
    visit {suc zero} (() ∷ d) z
    visit {suc (suc n)} d (Leaf addr , c) = nothing
    visit {suc (suc n)} d (Node a sh , c) = 
      seek d (sh , []) 
      >>= (λ { (Leaf addr , c₀) → nothing ; 
               (Node tr hsh , c₀) → just (tr , (a , hsh , c₀) ∷ c) })

    seek : {n : ℕ} → {A : Set} → Address n → Zipper n A → Maybe (Zipper n A)
    seek [] z = just z
    seek (d ∷ ds) z = seek ds z >>= visit d 

  seekTo : {n : ℕ} → {A : Set} → Address n → Tree n A → Maybe (Zipper n A)
  seekTo {zero} addr tr = seek addr (tr , tt)
  seekTo {suc n} addr tr = seek addr (tr , [])

  rootValue : {n : ℕ} → {A : Set} → Tree n A → Maybe A
  rootValue {zero} (Pt a) = just a
  rootValue {suc n} (Leaf addr) = nothing
  rootValue {suc n} (Node a sh) = just a

  _valueAt_ : {n : ℕ} → {A : Set} → Tree n A → Address n → Maybe A
  tr valueAt addr = seekTo addr tr >>= (λ z → rootValue (proj₁ z))

  graft : {n : ℕ} → {A : Set} → Tree (suc n) A → Tree n (Tree (suc n) A) → Maybe (Tree (suc n) A)
  graft (Leaf addr) brs = brs valueAt addr
  graft (Node a sh) brs = 
    traverse-tree maybeA (λ tr → graft tr brs) sh 
    >>= (λ r → just (Node a r))

  join : {n : ℕ} → {A : Set} → Tree n (Tree n A) → Maybe (Tree n A)
  join {zero} (Pt (Pt a)) = just (Pt a)
  join {suc n} (Leaf addr) = just (Leaf addr)
  join {suc n} (Node tr tsh) = traverse-tree maybeA join tsh >>= graft tr
