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
    pt : (a : A) → Tree₀ A 0
    leaf : {n : ℕ} → (addr : Address n) → Tree₀ A (suc n)
    node : {n : ℕ} → (a : A) → (sh : Tree₀ (Tree₀ A (suc n)) n) → Tree₀ A (suc n)

  Tree : ℕ → Set → Set
  Tree n A = Tree₀ A n

  map-tree : {n : ℕ} → {A B : Set} → (A → B) → Tree n A → Tree n B
  map-tree {zero} f (pt a) = pt (f a)
  map-tree {suc n} f (leaf addr) = leaf addr
  map-tree {suc n} f (node a sh) = node (f a) (map-tree (map-tree f) sh)

  traverse-tree : {n : ℕ} → {A B : Set} → {G : Set → Set} → Applicative G → (A → G B) → Tree n A → G (Tree n B)
  traverse-tree {zero} apG f (pt a) = let open Applicative apG in pure pt ⊛ f a
  traverse-tree {suc n} apG f (leaf addr) = let open Applicative apG in pure (leaf addr)
  traverse-tree {suc n} apG f (node a sh) = let open Applicative apG in 
    pure node ⊛ f a ⊛ traverse-tree apG (traverse-tree apG f) sh

  TreeF : (n : ℕ) → Functor (Tree n)
  TreeF n = record { fmap = map-tree {n} }

  TreeT : (n : ℕ) → Traverse (Tree n)
  TreeT n = record { isFunctor = TreeF n ; traverse = traverse-tree {n} }

  open Monad maybeM hiding (fmap)

  zipWithPrefix : {n : ℕ} → {A : Set} → Address n → Tree n A → Tree n (A × Address n)
  zipWithPrefix {zero} pref (pt a) = pt (a , [])
  zipWithPrefix {suc n} pref (leaf addr) = leaf addr
  zipWithPrefix {suc n} pref (node a sh) = 
    node (a , pref) (map-tree (λ { (t , d) → zipWithPrefix (d ∷ pref) t }) (zipWithPrefix [] sh))

  zipWithAddress : {n : ℕ} → {A : Set} → Tree n A → Tree n (A × Address n)
  zipWithAddress = zipWithPrefix []

  addrTreeWithPrefix : {n : ℕ} → {A : Set} → Address n → Tree n A → Tree n (Address n)
  addrTreeWithPrefix {zero} pref (pt a) = pt []
  addrTreeWithPrefix {suc n} pref (leaf addr) = leaf addr
  addrTreeWithPrefix {suc n} pref (node a sh) = 
    node pref (map-tree (λ { (t , a) → addrTreeWithPrefix (a ∷ pref) t }) (zipWithAddress sh))

  addrTree : {n : ℕ} → {A : Set} → Tree n A → Tree n (Address n)
  addrTree = addrTreeWithPrefix []

  zipComplete : {n : ℕ} → {A B : Set} → Tree n A → Tree n B → Maybe (Tree n (A × B))
  zipComplete {zero} (pt a) (pt b) = just (pt (a , b))
  zipComplete {suc n} (leaf l) (node b bsh) = nothing
  zipComplete {suc n} (node a ash) (leaf m) = nothing
  zipComplete {suc n} (leaf l) (leaf m) = just (leaf l)
  zipComplete {suc n} (node a ash) (node b bsh) = 
    zipComplete ash bsh 
    >>= (λ zsh → traverse-tree maybeA (uncurry zipComplete) zsh 
    >>= (λ sh → just (node (a , b) sh)))

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
    _←_ {zero} tt a = pt a
    _←_ {suc n} (sh , context) a = context ↓ node a sh

    _↓_ : {n : ℕ} → {A : Set} → Context n A → Tree n A → Tree n A
    _↓_ {zero} tt t = t
    _↓_ {suc n} [] t = t
    _↓_ {suc n} ((a , d) ∷ c) t = c ↓ node a (d ← t)

    visit : {n : ℕ} → {A : Set} → Direction n → Zipper n A → Maybe (Zipper n A)
    visit {zero} () z
    visit {suc zero} [] (leaf addr , c) = nothing
    visit {suc zero} [] (node head (pt tail) , c) = just (tail , (head , tt) ∷ c)
    visit {suc zero} (() ∷ d) z
    visit {suc (suc n)} d (leaf addr , c) = nothing
    visit {suc (suc n)} d (node a sh , c) = 
      seek d (sh , []) 
      >>= (λ { (leaf addr , c₀) → nothing ; 
               (node tr hsh , c₀) → just (tr , (a , hsh , c₀) ∷ c) })

    seek : {n : ℕ} → {A : Set} → Address n → Zipper n A → Maybe (Zipper n A)
    seek [] z = just z
    seek (d ∷ ds) z = seek ds z >>= visit d 

  seekTo : {n : ℕ} → {A : Set} → Address n → Tree n A → Maybe (Zipper n A)
  seekTo {zero} addr tr = seek addr (tr , tt)
  seekTo {suc n} addr tr = seek addr (tr , [])

  rootValue : {n : ℕ} → {A : Set} → Tree n A → Maybe A
  rootValue {zero} (pt a) = just a
  rootValue {suc n} (leaf addr) = nothing
  rootValue {suc n} (node a sh) = just a

  _valueAt_ : {n : ℕ} → {A : Set} → Tree n A → Address n → Maybe A
  tr valueAt addr = seekTo addr tr >>= (λ z → rootValue (proj₁ z))

  graft : {n : ℕ} → {A : Set} → Tree (suc n) A → Tree n (Tree (suc n) A) → Maybe (Tree (suc n) A)
  graft (leaf addr) brs = brs valueAt addr
  graft (node a sh) brs = 
    traverse-tree maybeA (λ tr → graft tr brs) sh 
    >>= (λ r → just (node a r))

  join : {n : ℕ} → {A : Set} → Tree n (Tree n A) → Maybe (Tree n A)
  join {zero} (pt (pt a)) = just (pt a)
  join {suc n} (leaf addr) = just (leaf addr)
  join {suc n} (node tr tsh) = traverse-tree maybeA join tsh >>= graft tr

  extentsSetupPrefix : {n : ℕ} → {A : Set} → Address n → Tree n A → Tree n (Address n × Derivative n (Address (suc n)) × A)
  extentsSetupPrefix {zero} addr (pt a) = pt ([] , tt , a)
  extentsSetupPrefix {suc n} addr (leaf l) = leaf l
  extentsSetupPrefix {suc n} {A} addr (node a shell) = 
    let shellInfo : Tree n (Address n × Derivative n (Address (suc n)) × Tree (suc n) A)
        shellInfo = extentsSetupPrefix [] shell
    in node (addr , (map-tree (λ { (d , _ , _) → leaf d }) shellInfo , []) , a) 
            (map-tree (λ { (d , ∂ , t) → extentsSetupPrefix (d ∷ addr) t }) shellInfo)

  extentsSetup : {n : ℕ} → {A : Set} → Tree n A → Tree n (Address n × Derivative n (Address (suc n)) × A)
  extentsSetup tr = extentsSetupPrefix [] tr

  corollaSetup : {n : ℕ} → {A : Set} → Tree n A → Tree n (Derivative n (Address n) × A)
  corollaSetup {zero} (pt a) = pt (tt , a)
  corollaSetup {suc n} (leaf l) = leaf l
  corollaSetup {suc n} (node a shell) = 
    node ((map-tree (λ { (_ , addr) → leaf addr }) (zipWithAddress shell) , []) , a) 
         (map-tree corollaSetup shell)

  mutual

    flattenWithPrefix : {n : ℕ} → {A : Set} → Address (suc n) → Derivative n (Address (suc n)) → Tree (suc n) A → Maybe (Tree n (Address (suc n)))
    flattenWithPrefix {n} addr ∂ (leaf l) = just (∂ ← addr)
    flattenWithPrefix {n} addr _ (node a shell) = shellExtentsPrefix addr shell

    shellExtentsPrefix : {n : ℕ} → {A : Set} → Address (suc n) → Tree n (Tree (suc n) A) → Maybe (Tree n (Address (suc n)))
    shellExtentsPrefix {n} addr shell =
      traverse maybeA (λ { (dir , deriv , tr) → flattenWithPrefix (dir ∷ addr) deriv tr }) (extentsSetup shell) 
      >>= join
      where open Traverse (TreeT n)

    shellExtents : {n : ℕ} → {A : Set} → Tree n (Tree (suc n) A) → Maybe (Tree n (Address (suc n)))
    shellExtents shell = shellExtentsPrefix [] shell

    treeCorolla : {n : ℕ} → {A : Set} → Derivative n (Address n) → Tree (suc n) A → Maybe (Tree n (Address n))
    treeCorolla ∂ (leaf l) = just (∂ ← l)
    treeCorolla ∂ (node a shell) = shellCorolla shell

    shellCorolla : {n : ℕ} → {A : Set} → Tree n (Tree (suc n) A) → Maybe (Tree n (Address n))
    shellCorolla {n} shell = 
      traverse maybeA (λ { (d , t) → treeCorolla d t }) (corollaSetup shell) >>= join
      where open Traverse (TreeT n)



  
