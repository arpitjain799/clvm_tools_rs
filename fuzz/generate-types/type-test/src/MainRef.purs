module MainRef where

import Prelude
import Data.Maybe
import Data.Tuple
import Effect (Effect)
import Effect.Console (log)

data ChiaPrim = ChiaAtom String | ChiaCons ChiaPrim ChiaPrim

{- These are type level representations, but the actual data is a chia value
 - This simulates the conversion of the program from typed to untyped.
 -}
data Nil = Nil ChiaPrim
data Atom = Atom ChiaPrim
data Atom32 = Atom32 ChiaPrim
data Nullable a = Nullable ChiaPrim
data Pair a b = Pair ChiaPrim
data Exec x = Exec ChiaPrim
data Any = Any ChiaPrim
data ChiaFun a b = ChiaFun ChiaPrim

-- Monad for chia
data ChiaOutcome t = ChiaResult t | ChiaException ChiaPrim

instance chiaOutcomeFunctor :: Functor ChiaOutcome where
  map :: forall a b. (a -> b) -> ChiaOutcome a -> ChiaOutcome b
  map f (ChiaResult v) = ChiaResult $ (f v)
  map _ (ChiaException e) = ChiaException e

instance chiaOutcomeApplicative :: Applicative ChiaOutcome where
  pure :: forall a. a -> ChiaOutcome a
  pure x = ChiaResult x

instance chiaOutcomeApply :: Apply ChiaOutcome where
  apply :: forall a b. ChiaOutcome (a -> b) -> ChiaOutcome a -> ChiaOutcome b
  apply (ChiaResult f) (ChiaResult a) = ChiaResult $ f a
  apply (ChiaException e) _ = ChiaException e
  apply _ (ChiaException e) = ChiaException e

class HasValue t where
  getValue :: t -> ChiaPrim

instance chiaPrimHasValue :: HasValue ChiaPrim where
  getValue p = p

instance chiaNilHasValue :: HasValue Nil where
  getValue (Nil v) = v

instance chiaAtomHasValue :: HasValue Atom where
  getValue (Atom v) = v

instance chiaAtom32HasValue :: HasValue Atom32 where
  getValue (Atom32 v) = v

instance chiaNullableHasValue :: HasValue (Nullable a) where
  getValue (Nullable v) = v

instance chiaPairHasValue :: HasValue (Pair a b) where
  getValue (Pair v) = v

instance chiaExecHasValue :: HasValue (Exec x) where
  getValue (Exec v) = v

instance chiaAnyHasValue :: HasValue Any where
  getValue (Any v) = v

instance chiaFunHasValue :: HasValue (ChiaFun a b) where
  getValue (ChiaFun v) = v

class FromValue t where
  fromValue :: ChiaPrim -> t

instance chiaPrimFromValue :: FromValue ChiaPrim where
  fromValue p = p

instance chiaNilFromValue :: FromValue Nil where
  fromValue p = Nil p

instance chiaAtomFromValue :: FromValue Atom where
  fromValue p = Atom p

instance chiaAtom32FromValue :: FromValue Atom32 where
  fromValue p = Atom32 p

instance chiaNullableFromValue :: FromValue (Nullable a) where
  fromValue p = Nullable p

instance chiaPairFromValue :: FromValue (Pair a b) where
  fromValue p = Pair p

instance chiaExecFromValue :: FromValue (Exec x) where
  fromValue p = Exec p

instance chiaAnyFromValue :: FromValue Any where
  fromValue p = Any p

instance chiaFunFromValue :: FromValue (ChiaFun a b) where
  fromValue p = ChiaFun p

-- The following constraints are used to implement the basic subtype ladder.
class Convert x y where
  cvt :: x -> y

instance cvtNilNil :: Convert Nil Nil where
  cvt (Nil x) = Nil x

instance cvtAnyNil :: Convert Any Nil where
  cvt (Any x) = Nil x

instance cvtNullableXNil :: Convert (Nullable x) Nil where
  cvt (Nullable x) = Nil x

instance cvtXNullableX :: (HasValue x) => Convert x (Nullable x) where
  cvt x = Nullable (getValue x)

instance cvtNilAny :: Convert Nil Any where
  cvt (Nil x) = Any x

instance cvtAtomAny :: Convert Atom Any where
  cvt (Atom x) = Any x

instance cvtAtom32Any :: Convert Atom32 Any where
  cvt (Atom32 x) = Any x

instance cvtNullableXAny :: (Convert x Any) => Convert (Nullable x) Any where
  cvt (Nullable x) = Any x

instance cvtPairABAny :: (Convert a Any, Convert b Any) => Convert (Pair a b) Any where
  cvt (Pair x) = Any x

instance cvtAnyAny :: Convert Any Any where
  cvt (Any x) = Any x

instance cvtAtomAtom :: Convert Atom Atom where
  cvt (Atom x) = Atom x

instance cvtAtom32Atom :: Convert Atom32 Atom where
  cvt (Atom32 x) = Atom x

instance cvtNilAtom :: Convert Nil Atom where
  cvt (Nil x) = Atom x

instance cvtAnyAtom :: Convert Any Atom where
  cvt (Any x) = Atom x

instance cvtAtom32Atom32 :: Convert Atom32 Atom32 where
  cvt (Atom32 x) = Atom32 x

instance cvtAnyAtom32 :: Convert Any Atom32 where
  cvt (Any x) = Atom32 x

instance cvtPairABPairXY :: (Convert a x, Convert b y) => Convert (Pair a b) (Pair x y) where
  cvt (Pair x) = Pair x

instance cvtAnyPairAB :: (Convert Any a, Convert Any b) => Convert Any (Pair a b) where
  cvt (Any x) = Pair x

instance cvtAnyExecX :: (Convert Any x) => Convert Any (Exec x) where
  cvt (Any x) = Exec x

instance cvtAnyChiaFunAB :: Convert Any (ChiaFun a b) where
  cvt (Any x) = ChiaFun x

class ListOfAtoms x where
  unroll :: x -> Maybe (Tuple Atom x)

instance listOfAtomsNil :: ListOfAtoms Nil where
  unroll (Nil x) = Nothing

instance listOfAtomsNullable :: (ListOfAtoms x, FromValue x) => ListOfAtoms (Nullable x) where
  unroll (Nullable v) = unroll (fromValue v)

instance listOfAtomsPair :: (FromValue b, ListOfAtoms b) => ListOfAtoms (Pair a b) where
  unroll (Pair (ChiaCons x y)) = Just (Tuple (fromValue x) (fromValue y))
  unroll (Pair _) = Nothing

c :: forall a b c. (HasValue a) => (HasValue b) => (Pair a (Pair b c)) -> ChiaOutcome (Pair a b)
c (Pair (ChiaCons a (ChiaCons b _))) = pure $ Pair (ChiaCons (getValue a) (getValue b))
c x = ChiaException (getValue x)

sha256 :: forall x. (ListOfAtoms x) => x -> ChiaOutcome Atom32
sha256 _ = ChiaResult $ Atom32 (ChiaAtom "")

multiply :: forall x. (ListOfAtoms x) => x -> ChiaOutcome Atom
multiply _ = ChiaResult $ Atom (ChiaAtom "")

subtract :: Pair Atom (Pair Atom Unit) -> ChiaOutcome Atom
subtract _ = ChiaResult $ Atom (ChiaAtom "")

f :: forall f0 r0. (FromValue f0) => (Pair (Pair f0 r0) Nil) -> ChiaOutcome f0
f (Pair (ChiaCons (ChiaCons a b) _)) = ChiaResult (fromValue a)
f x = ChiaException (getValue x)

truthy :: ChiaPrim -> Boolean
truthy _ = false

a :: forall a b c. (HasValue (Exec (ChiaFun a b))) => (FromValue b) => (Pair (Exec (ChiaFun a b)) (Pair b c)) -> ChiaOutcome b
a x = pure $ fromValue (getValue x)

i :: forall c a b q x.  (FromValue a) => (FromValue b) => (Convert a x) => (Convert b x) => (Convert c Any) => (Pair c (Pair a (Pair b q))) -> ChiaOutcome x
i (Pair (ChiaCons c (ChiaCons a (ChiaCons b _)))) =
  let
    a_converted :: a
    a_converted = fromValue a

    b_converted :: b
    b_converted = fromValue b
  in
  pure $ if truthy c then cvt a_converted else cvt b_converted
i x = ChiaException (getValue x)

main :: Effect Unit
main = do
  log "Hello sailor!"
