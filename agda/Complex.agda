--
--  Complex.agda - Complexes
--

{-# OPTIONS --no-termination-check #-}

open import Prelude
open import Mtl
open import Tree
open import Nesting
open import Suite

module Complex where

  open Monad maybeM hiding (fmap ; η ; μ)

  Complex : (ℕ → Set) → ℕ → Set
  Complex A n = Suite (λ k → Nesting k (A k)) n

  ComplexZipper : (ℕ → Set) → ℕ → Set
  ComplexZipper A n = Suite (λ k → ZipperNst k (A k)) n

  toZipper : {n : ℕ} → {A : ℕ → Set} → Complex A n → ComplexZipper A n
  toZipper {zero} (■ bs) = ■ (bs , [])
  toZipper {suc n} (tl ⟫ hd) = toZipper tl ⟫ (hd , [])

  seal : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A n → Complex A n
  seal {zero} (■ (nst , cntx)) = ■ (close cntx nst)
  seal {suc n} (cz ⟫ (nst , cntx)) = seal cz ⟫ close cntx nst

  updateFocus : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A n → Nesting n (A n) → ComplexZipper A n
  updateFocus {zero} (■ (_ , cntx)) nst = ■ (nst , cntx)
  updateFocus {suc n} (c ⟫ (_ , cntx)) nst = c ⟫ (nst , cntx)

  focusValue : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A n → A n
  focusValue c = base-value (proj₁ (head c))

  focusCorolla : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A (suc n) → Maybe (Tree n (Address n))
  focusCorolla {zero} _ = just (pt [])
  focusCorolla {suc n} (zc ⟫ (fcs , cntx)) = spine fcs 
    >>= (λ { (leaf addr) → focusCorolla zc >>= (λ cr → just (node [] (map-tree leaf cr))) ; 
             (node a sh) → shellCorolla sh })

  focusUnit : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A n → Maybe (Tree n (Nesting n (A n)))
  focusUnit {zero} (■ (nst , cntx)) = just (pt nst)
  focusUnit {suc n} (zc ⟫ z) = 
    focusCorolla (zc ⟫ z) >>= (λ cr → just (node (proj₁ z) (map-tree leaf cr)))

  focusCanopy : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A n → Maybe (Tree n (Address n))
  focusCanopy {zero} (■ _) = just (pt [])
  focusCanopy {suc n} (zc ⟫ (ext a cr , cntx)) = nothing
  focusCanopy {suc n} (zc ⟫ (int a sh , cntx)) = just (addrTree sh)

  mutual

    visit-cmplx : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A n → Address n → Maybe (ComplexZipper A n)
    visit-cmplx {zero} (■ bs) addr = visit-nesting addr bs >>= (λ r → just (■ r))
    visit-cmplx {suc n} (tl ⟫ hd) [] = visit-nesting [] hd >>= (λ r → just (tl ⟫ r))
    visit-cmplx {suc n} zc (d ∷ ds) = 
      visit-cmplx {suc n} zc ds 
      >>= (λ { (zc₀ ⟫ (pd , cn)) → sibling d (pd , cn) 
      >>= (λ sz → spine pd 
      >>= (λ { (leaf addr) → just (zc₀ ⟫ sz) ;   -- The recusive call has left us on a drop.  The lower dimensions should already be set.
               (node a sh) → shellExtents sh     -- The extents give us all the options for where we might have to go ...
                             >>= (λ extents → extents valueAt d  -- ... and we choose the one for the direction we want
                             >>= (λ recAddr → seek-cmplx {n} zc₀ recAddr 
                             >>= (λ zc₁ → just (zc₁ ⟫ sz)))) })) }) 

    seek-cmplx : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A n → Address (suc n) → Maybe (ComplexZipper A n)
    seek-cmplx {n} zc [] = just zc
    seek-cmplx {n} zc (d ∷ ds) = seek-cmplx {n} zc ds >>= (λ zc₀ → visit-cmplx {n} zc₀ d)


  SourceM : (n : ℕ) → (ℕ → Set) → Set → Set
  SourceM n A = StateT Maybe (Complex A n)

  SourceMN : (n : ℕ) → (A : ℕ → Set) → Monad (SourceM n A)
  SourceMN n A = stateTM maybeM

  SourceMS : (n : ℕ) → (A : ℕ → Set) → MonadState (SourceM n A) (Complex A n)
  SourceMS n A = stateTS maybeM

  SourceAP : (n : ℕ) → (A : ℕ → Set) → Applicative (SourceM n A)
  SourceAP n A = monadIsApp (SourceMN n A)

  liftS : {n : ℕ} → {A : ℕ → Set} → {B : Set} → Maybe B → SourceM n A B
  liftS nothing = toS (λ c → nothing)
  liftS (just b) = toS (λ c → just (c , b))

  mutual

    sourceAt : {n : ℕ} → {A : ℕ → Set} → Complex A n → Address (suc n) → Maybe (Complex A n)
    sourceAt {n} c addr = restrictAt {n} c addr >>= (λ c₀ → contractAt {n} c₀ [])

    restrictAt : {n : ℕ} → {A : ℕ → Set} → Complex A n → Address (suc n) → Maybe (Complex A n)
    restrictAt {n} c addr = 
      seek-cmplx {n} (toZipper {n} c) addr 
      >>= (λ zc → restrictFocus {n} zc 
      >>= (λ zc₀ → just (seal {n} zc₀)))

    contractAt : {n : ℕ} → {A : ℕ → Set} → Complex A n → Address (suc n) → Maybe (Complex A n)
    contractAt {n} c addr = 
      seek-cmplx {n} (toZipper {n} c) addr 
      >>= (λ zc → contractFocus {n} zc 
      >>= (λ zc₀ → just (seal {n} zc₀)))

    restrictFocus : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A n → Maybe (ComplexZipper A n)
    restrictFocus {zero} (■ (pd , cn)) = just (■ (pd , []))
    restrictFocus {suc n} (zc ⟫ (pd , cn)) = 
      spine pd 
      >>= (λ fpd → restrictFocus {n} zc 
      >>= (λ zc₀ → eval-stateT maybeF (seal {n} zc₀) (exciseLocal [] fpd) 
      >>= (λ nc → just (toZipper {n} nc ⟫ (pd , [])) )))

    contractFocus : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A n → Maybe (ComplexZipper A n)
    contractFocus {zero} (■ (pd , cn)) = just (■ (obj (base-value pd) , cn))
    contractFocus {suc n} (zc ⟫ (pd , cn)) = 
      spine pd 
      >>= (λ fpd → compressFocus zc fpd 
      >>= (λ zc₀ → focusCorolla (zc ⟫ (pd , cn))
      >>= (λ cr → just (zc₀ ⟫ ((ext (base-value pd) cr) , cn)) )))

    compressFocus : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A n → Tree (suc n) (A (suc n)) → Maybe (ComplexZipper A n)
    compressFocus {n} zc tr = compressLocal {n} zc tr >>= (λ pd → just (updateFocus {n} zc (int (focusValue {n} zc) pd)))

    compressLocal : {n : ℕ} → {A : ℕ → Set} → ComplexZipper A n → Tree (suc n) (A (suc n)) → Maybe (Tree n (Nesting n (A n)))
    compressLocal {n} zc (leaf l) = focusUnit zc
    compressLocal {n} zc (node a shell) = 
      focusCanopy {n} zc 
      >>= (λ cp → zipComplete cp shell 
      >>= (λ zsh → traverse-tree maybeA 
             (λ { (d , tr) → visit-cmplx {n} zc d >>= (λ zc₀ → compressLocal zc₀ tr) }) zsh 
                             >>= join))

    exciseLocal : {n : ℕ} → {A : ℕ → Set} → Address (suc n) → Tree (suc n) (A (suc n)) → SourceM n A ⊤
    exciseLocal {n} {A} addr (leaf l) = 
      get >>=ₛ (λ c → liftS {n} (contractAt {n} c addr) >>=ₛ put) 
      where open MonadState (SourceMS n A) renaming (_>>=_ to _>>=ₛ_)
    exciseLocal {n} {A} addr (node a shell) = 
      traverse-tree (SourceAP n A) (λ { (t , d) → exciseLocal (d ∷ addr) t }) (zipWithAddress shell)
      >>=ₛ (λ _ → η tt)
      where open MonadState (SourceMS n A) renaming (_>>=_ to _>>=ₛ_) 

  -- Okay, the idea is to work on comultiplication.  What does this mean?  The idea is to 
  -- put the complex representing the face of each face of a complex on that face. Ha!

  -- And somehow, I think we want to use zippers to do it.

  comult : {n : ℕ} → {A : ℕ → Set} → Complex A n → Maybe (Complex (Complex A) n)
  comult {zero} (■ (obj ob)) = just (■ (obj (■ (obj ob))))
  comult {zero} (■ (int a (pt nst))) = 
    comult (■ nst) >>= (λ { (■ res) → just (■ (int (■ (obj a)) (pt res))) })
  comult {suc n} (ic ⟫ nst) = 
    traverse-nesting maybeA (λ { (_ , addr) → sourceAt (ic ⟫ nst) addr }) (nestingWithAddr nst) 
    >>= (λ hd → comult ic 
    >>= (λ tl → just (tl ⟫ hd)))


