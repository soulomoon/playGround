module dp where

data Bool : Set where
     true  : Bool
     false : Bool

--
-- _or_ : Bool -> Bool -> Bool
-- false or x = x
-- true or_=true

if_then_else_ : {A : Set} -> Bool -> A -> A -> A
if true then x else y = x
if false then x else y = y

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

data Vec (A : Set) : Nat -> Set where
  [] : Vec A zero
  _::_ : {n : Nat} -> A -> Vec A n -> Vec A (suc n)

data Vec2 (A : Set) : Nat -> Set where
  nil : Vec2 A zero
  cons : (n : Nat)-> A -> Vec2 A n -> Vec2 A (suc n)

vmap : {A B : Set}{n : Nat} -> (A -> B) -> Vec A n -> Vec B n
vmap f [] = []
vmap f (x :: xs) = f x :: vmap f xs

vmap2 :{A B : Set}(n : Nat) -> (A -> B) -> Vec2 A n -> Vec2 B n
vmap2 .zero f nil = nil
vmap2 .(suc n) f (cons n x xs) = cons n (f x) (vmap2 n f xs)

vmap3 :{A B : Set}(n : Nat) -> (A -> B) -> Vec2 A n -> Vec2 B n
vmap3 zero f nil = nil
vmap3 (suc n) f (cons .n x xs) = cons n (f x) (vmap2 n f xs)


data Image_∋_ {A B : Set}(f : A -> B) : B -> Set where
  im : (x : A) -> Image f ∋ f x

inv : {A B : Set}(f : A -> B)(y : B) -> Image f ∋ y -> A
inv f .(f x) (im x) = x


data Fin : Nat -> Set where
  fzero : {n : Nat} -> Fin (suc n)
  fsuc : {n : Nat} -> Fin n -> Fin (suc n)

magic : {A : Set} -> Fin zero -> A
magic ()

data Empty : Set where
  empty : Fin zero -> Empty

magic’ : {A : Set} -> Empty -> A
magic’ (empty ())


_◦_ : {A : Set}{B : A -> Set}{C : (x : A) -> B x -> Set}
      (f : {x : A}(y : B x) -> C x y)(g : (x : A) -> B x)
      (x : A) -> C x (g x)
(f ◦ g) x = f (g x)

_!_ : {n : Nat}{A : Set} -> Vec A n -> Fin n -> A
[] ! ()
(x :: xs) ! fzero = x
(x :: xs) ! (fsuc i) = xs ! i

-- tabulate : {n : Nat}{A : Set} -> (Fin n -> A) -> Vec A n
-- tabulate {zero} f = []
-- tabulate {suc n} f = f fzero :: tabulate (f ◦ fsuc)

tabulate : {n : Nat}{A : Set} -> (Fin n -> A) -> Vec A n
tabulate {zero} f = []
tabulate {suc n} f = f fzero :: tabulate (f ◦ fsuc)

tabulatef : {n : Nat} -> Fin n -> Fin n
tabulatef x = x
-- tabulatef (fsuc n) = suc (tabulatef {n} n)

tf : {n : Nat} -> Fin n -> Nat
tf fzero = suc zero
tf (fsuc n) = suc (tf n)

myList : {n : Nat} -> Vec (Fin n) n
myList = tabulate tabulatef

ls2 : {n : Nat} -> Vec Nat n
ls2 = tabulate tf
