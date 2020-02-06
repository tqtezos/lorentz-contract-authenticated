{-# OPTIONS -fmax-pmcheck-iterations=20000000 -Wno-missing-export-lists #-}

module Michelson.Typed.Annotation.Sing where

import Data.Kind
import GHC.TypeLits

import Lorentz
import Michelson.Typed.Annotation
import Michelson.Untyped.Annotation
import Michelson.Typed.T
import Lorentz.EntryPoints.Core

import Data.Singletons
import Data.Singletons.TypeLits
import Data.Constraint
import Fcf (Exp)
import qualified Fcf as Fcf

-- | A generalization of `Notes` that's more amenable to `Sing`, `SingKind`
data Annotated a t where
  ATc         :: a -> Annotated a ('Tc ct)
  ATKey       :: a -> Annotated a 'TKey
  ATUnit      :: a -> Annotated a 'TUnit
  ATSignature :: a -> Annotated a 'TSignature
  ATChainId   :: a -> Annotated a 'TChainId
  ATOption    :: a -> Annotated a t -> Annotated a ('TOption t)
  ATList      :: a -> Annotated a t -> Annotated a ('TList t)
  ATSet       :: a -> a -> Annotated a ('TSet ct)
  ATOperation :: a -> Annotated a 'TOperation
  ATContract  :: a -> Annotated a t -> Annotated a ('TContract t)
  ATPair      :: a -> a -> a
              -> Annotated a p -> Annotated a q -> Annotated a ('TPair p q)
  ATOr        :: a -> a -> a
              -> Annotated a p -> Annotated a q -> Annotated a ('TOr p q)
  ATLambda    :: a -> Annotated a p -> Annotated a q -> Annotated a ('TLambda p q)
  ATMap       :: a -> a -> Annotated a v -> Annotated a ('TMap k v)
  ATBigMap    :: a -> a -> Annotated a v -> Annotated a ('TBigMap k v)

-- | Convert to `Notes`
annotatedToNotes :: Annotated Text t -> Notes t
annotatedToNotes (ATc ta) = NTc (AnnotationUnsafe ta)
annotatedToNotes (ATKey ta) = NTKey (AnnotationUnsafe ta)
annotatedToNotes (ATUnit ta) = NTUnit (AnnotationUnsafe ta)
annotatedToNotes (ATSignature ta) = NTSignature (AnnotationUnsafe ta)
annotatedToNotes (ATChainId ta) = NTChainId (AnnotationUnsafe ta)
annotatedToNotes (ATOption ta xs) = NTOption (AnnotationUnsafe ta) (annotatedToNotes xs)
annotatedToNotes (ATList ta xs) = NTList (AnnotationUnsafe ta) (annotatedToNotes xs)
annotatedToNotes (ATSet ta tb) = NTSet (AnnotationUnsafe ta) (AnnotationUnsafe tb)
annotatedToNotes (ATOperation ta) = NTOperation (AnnotationUnsafe ta)
annotatedToNotes (ATContract ta xs) = NTContract (AnnotationUnsafe ta) (annotatedToNotes xs)
annotatedToNotes (ATPair ta tb tc xs ys) = NTPair (AnnotationUnsafe ta) (AnnotationUnsafe tb) (AnnotationUnsafe tc) (annotatedToNotes xs) (annotatedToNotes ys)
annotatedToNotes (ATOr ta tb tc xs ys) = NTOr (AnnotationUnsafe ta) (AnnotationUnsafe tb) (AnnotationUnsafe tc) (annotatedToNotes xs) (annotatedToNotes ys)
annotatedToNotes (ATLambda ta xs ys) = NTLambda (AnnotationUnsafe ta) (annotatedToNotes xs) (annotatedToNotes ys)
annotatedToNotes (ATMap ta tb xs) = NTMap (AnnotationUnsafe ta) (AnnotationUnsafe tb) (annotatedToNotes xs)
annotatedToNotes (ATBigMap ta tb xs) = NTBigMap (AnnotationUnsafe ta) (AnnotationUnsafe tb) (annotatedToNotes xs)

-- | Convert from `Notes`
annotatedFromNotes :: Notes t -> Annotated Text t
annotatedFromNotes (NTc (AnnotationUnsafe ta)) = ATc ta
annotatedFromNotes (NTKey (AnnotationUnsafe ta)) = ATKey ta
annotatedFromNotes (NTUnit (AnnotationUnsafe ta)) = ATUnit ta
annotatedFromNotes (NTSignature (AnnotationUnsafe ta)) = ATSignature ta
annotatedFromNotes (NTChainId (AnnotationUnsafe ta)) = ATChainId ta
annotatedFromNotes (NTOption (AnnotationUnsafe ta) xs) = ATOption ta (annotatedFromNotes xs)
annotatedFromNotes (NTList (AnnotationUnsafe ta) xs) = ATList ta (annotatedFromNotes xs)
annotatedFromNotes (NTSet (AnnotationUnsafe ta) (AnnotationUnsafe tb)) = ATSet ta tb
annotatedFromNotes (NTOperation (AnnotationUnsafe ta)) = ATOperation ta
annotatedFromNotes (NTContract (AnnotationUnsafe ta) xs) = ATContract ta (annotatedFromNotes xs)
annotatedFromNotes (NTPair (AnnotationUnsafe ta) (AnnotationUnsafe tb) (AnnotationUnsafe tc) xs ys) = ATPair ta tb tc (annotatedFromNotes xs) (annotatedFromNotes ys)
annotatedFromNotes (NTOr (AnnotationUnsafe ta) (AnnotationUnsafe tb) (AnnotationUnsafe tc) xs ys) = ATOr ta tb tc (annotatedFromNotes xs) (annotatedFromNotes ys)
annotatedFromNotes (NTLambda (AnnotationUnsafe ta) xs ys) = ATLambda ta (annotatedFromNotes xs) (annotatedFromNotes ys)
annotatedFromNotes (NTMap (AnnotationUnsafe ta) (AnnotationUnsafe tb) xs) = ATMap ta tb (annotatedFromNotes xs)
annotatedFromNotes (NTBigMap (AnnotationUnsafe ta) (AnnotationUnsafe tb) xs) = ATBigMap ta tb (annotatedFromNotes xs)

data instance Sing :: Annotated a t -> Type where
  SATc         :: forall a (ta :: a). Sing ta -> Sing ('ATc ta)
  SATKey       :: forall a (ta :: a). Sing ta -> Sing ('ATKey ta)
  SATUnit      :: forall a (ta :: a). Sing ta -> Sing ('ATUnit ta)
  SATSignature :: forall a (ta :: a). Sing ta -> Sing ('ATSignature ta)
  SATChainId   :: forall a (ta :: a). Sing ta -> Sing ('ATChainId ta)
  SATOption    :: forall a t (ta :: a) (xs :: Annotated a t). Sing ta -> Sing xs -> Sing ('ATOption ta xs)
  SATList      :: forall a t (ta :: a) (xs :: Annotated a t). Sing ta -> Sing xs -> Sing ('ATList ta xs)
  SATSet       :: forall a (ta :: a) (tb :: a). Sing ta -> Sing tb -> Sing ('ATSet ta tb)
  SATOperation :: forall a (ta :: a). Sing ta -> Sing ('ATOperation ta)
  SATContract  :: forall a t (ta :: a) (xs :: Annotated a t). Sing ta -> Sing xs -> Sing ('ATContract ta xs)
  SATPair      :: forall a s t (ta :: a) (tb :: a) (tc :: a) (xs :: Annotated a s) (ys :: Annotated a t). Sing ta -> Sing tb -> Sing tc -> Sing xs -> Sing ys -> Sing ('ATPair ta tb tc xs ys)
  SATOr        :: forall a s t (ta :: a) (tb :: a) (tc :: a) (xs :: Annotated a s) (ys :: Annotated a t). Sing ta -> Sing tb -> Sing tc -> Sing xs -> Sing ys -> Sing ('ATOr ta tb tc xs ys)
  SATLambda    :: forall a s t (ta :: a) (xs :: Annotated a s) (ys :: Annotated a t). Sing ta -> Sing xs -> Sing ys -> Sing ('ATLambda ta xs ys)
  SATMap       :: forall a t (ta :: a) (tb :: a) (xs :: Annotated a t). Sing ta -> Sing tb -> Sing xs -> Sing ('ATMap ta tb xs)
  SATBigMap    :: forall a t (ta :: a) (tb :: a) (xs :: Annotated a t). Sing ta -> Sing tb -> Sing xs -> Sing ('ATBigMap ta tb xs)

-- instance SingI ta => SingI ('ATc ta) where
--   sing = SATc sing


instance (SingI ta) => SingI ('ATc ta) where
  sing = SATc sing
instance (SingI ta) => SingI ('ATKey ta) where
  sing = SATKey sing
instance (SingI ta) => SingI ('ATUnit ta) where
  sing = SATUnit sing
instance (SingI ta) => SingI ('ATSignature ta) where
  sing = SATSignature sing
instance (SingI ta) => SingI ('ATChainId ta) where
  sing = SATChainId sing
instance (SingI ta,  SingI xs) => SingI ('ATOption ta xs) where
  sing = SATOption sing sing
instance (SingI ta,  SingI xs) => SingI ('ATList ta xs) where
  sing = SATList sing sing
instance (SingI ta,  SingI tb) => SingI ('ATSet ta tb) where
  sing = SATSet sing sing
instance (SingI ta) => SingI ('ATOperation ta) where
  sing = SATOperation sing
instance (SingI ta,  SingI xs) => SingI ('ATContract ta xs) where
  sing = SATContract sing sing
instance (SingI ta,  SingI tb,  SingI tc,  SingI xs,  SingI ys) => SingI ('ATPair ta tb tc xs ys) where
  sing = SATPair sing sing sing sing sing
instance (SingI ta,  SingI tb,  SingI tc,  SingI xs,  SingI ys) => SingI ('ATOr ta tb tc xs ys) where
  sing = SATOr sing sing sing sing sing
instance (SingI ta,  SingI xs,  SingI ys) => SingI ('ATLambda ta xs ys) where
  sing = SATLambda sing sing sing
instance (SingI ta,  SingI tb,  SingI xs) => SingI ('ATMap ta tb xs) where
  sing = SATMap sing sing sing
instance (SingI ta,  SingI tb,  SingI xs) => SingI ('ATBigMap ta tb xs) where
  sing = SATBigMap sing sing sing

-- | A proof that `Sing` implies `SingI` for `Annotated`, if it does for @a@
singIAnnotated :: forall a t (xs :: Annotated a t). (forall (x :: a). Sing x -> Dict (SingI x)) -> Sing xs -> Dict (SingI xs)
singIAnnotated singIA (SATc ta) =
  case (singIA ta) of
    (Dict) -> Dict
singIAnnotated singIA (SATKey ta) =
  case (singIA ta) of
    (Dict) -> Dict
singIAnnotated singIA (SATUnit ta) =
  case (singIA ta) of
    (Dict) -> Dict
singIAnnotated singIA (SATSignature ta) =
  case (singIA ta) of
    (Dict) -> Dict
singIAnnotated singIA (SATChainId ta) =
  case (singIA ta) of
    (Dict) -> Dict
singIAnnotated singIA (SATOption ta xs) =
  case (singIA ta, singIAnnotated singIA xs) of
    (Dict, Dict) -> Dict
singIAnnotated singIA (SATList ta xs) =
  case (singIA ta, singIAnnotated singIA xs) of
    (Dict, Dict) -> Dict
singIAnnotated singIA (SATSet ta tb) =
  case (singIA ta, singIA tb) of
    (Dict, Dict) -> Dict
singIAnnotated singIA (SATOperation ta) =
  case (singIA ta) of
    (Dict) -> Dict
singIAnnotated singIA (SATContract ta xs) =
  case (singIA ta, singIAnnotated singIA xs) of
    (Dict, Dict) -> Dict
singIAnnotated singIA (SATPair ta tb tc xs ys) =
  case (singIA ta, singIA tb, singIA tc, singIAnnotated singIA xs, singIAnnotated singIA ys) of
    (Dict, Dict, Dict, Dict, Dict) -> Dict
singIAnnotated singIA (SATOr ta tb tc xs ys) =
  case (singIA ta, singIA tb, singIA tc, singIAnnotated singIA xs, singIAnnotated singIA ys) of
    (Dict, Dict, Dict, Dict, Dict) -> Dict
singIAnnotated singIA (SATLambda ta xs ys) =
  case (singIA ta, singIAnnotated singIA xs, singIAnnotated singIA ys) of
    (Dict, Dict, Dict) -> Dict
singIAnnotated singIA (SATMap ta tb xs) =
  case (singIA ta, singIA tb, singIAnnotated singIA xs) of
    (Dict, Dict, Dict) -> Dict
singIAnnotated singIA (SATBigMap ta tb xs) =
  case (singIA ta, singIA tb, singIAnnotated singIA xs) of
    (Dict, Dict, Dict) -> Dict

-- | A proof that `Sing` implies `SingI` for `Symbol`
singISymbol :: forall (x :: Symbol). Sing x -> Dict (SingI x)
singISymbol SSym = Dict

instance SingKind a => SingKind (Annotated a t) where
  type Demote (Annotated a t) = Annotated (Demote a) t

  fromSing (SATc ta) = ATc (fromSing ta)
  fromSing (SATKey ta) = ATKey (fromSing ta)
  fromSing (SATUnit ta) = ATUnit (fromSing ta)
  fromSing (SATSignature ta) = ATSignature (fromSing ta)
  fromSing (SATChainId ta) = ATChainId (fromSing ta)
  fromSing (SATOption ta xs) = ATOption (fromSing ta) (fromSing xs)
  fromSing (SATList ta xs) = ATList (fromSing ta) (fromSing xs)
  fromSing (SATSet ta tb) = ATSet (fromSing ta) (fromSing tb)
  fromSing (SATOperation ta) = ATOperation (fromSing ta)
  fromSing (SATContract ta xs) = ATContract (fromSing ta) (fromSing xs)
  fromSing (SATPair ta tb tc xs ys) = ATPair (fromSing ta) (fromSing tb) (fromSing tc) (fromSing xs) (fromSing ys)
  fromSing (SATOr ta tb tc xs ys) = ATOr (fromSing ta) (fromSing tb) (fromSing tc) (fromSing xs) (fromSing ys)
  fromSing (SATLambda ta xs ys) = ATLambda (fromSing ta) (fromSing xs) (fromSing ys)
  fromSing (SATMap ta tb xs) = ATMap (fromSing ta) (fromSing tb) (fromSing xs)
  fromSing (SATBigMap ta tb xs) = ATBigMap (fromSing ta) (fromSing tb) (fromSing xs)

  toSing (ATc ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATc sta
  toSing (ATKey ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATKey sta
  toSing (ATUnit ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATUnit sta
  toSing (ATSignature ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATSignature sta
  toSing (ATChainId ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATChainId sta
  toSing (ATOption ta xs) =
    case (toSing ta, toSing xs) of
      (SomeSing sta, SomeSing sxs) ->
        SomeSing $
        SATOption sta sxs
  toSing (ATList ta xs) =
    case (toSing ta, toSing xs) of
      (SomeSing sta, SomeSing sxs) ->
        SomeSing $
        SATList sta sxs
  toSing (ATSet ta tb) =
    case (toSing ta, toSing tb) of
      (SomeSing sta, SomeSing stb) ->
        SomeSing $
        SATSet sta stb
  toSing (ATOperation ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATOperation sta
  toSing (ATContract ta xs) =
    case (toSing ta, toSing xs) of
      (SomeSing sta, SomeSing sxs) ->
        SomeSing $
        SATContract sta sxs
  toSing (ATPair ta tb tc xs ys) =
    case (toSing ta, toSing tb, toSing tc, toSing xs, toSing ys) of
      (SomeSing sta, SomeSing stb, SomeSing stc, SomeSing sxs, SomeSing sys) ->
        SomeSing $
        SATPair sta stb stc sxs sys
  toSing (ATOr ta tb tc xs ys) =
    case (toSing ta, toSing tb, toSing tc, toSing xs, toSing ys) of
      (SomeSing sta, SomeSing stb, SomeSing stc, SomeSing sxs, SomeSing sys) ->
        SomeSing $
        SATOr sta stb stc sxs sys
  toSing (ATLambda ta xs ys) =
    case (toSing ta, toSing xs, toSing ys) of
      (SomeSing sta, SomeSing sxs, SomeSing sys) ->
        SomeSing $
        SATLambda sta sxs sys
  toSing (ATMap ta tb xs) =
    case (toSing ta, toSing tb, toSing xs) of
      (SomeSing sta, SomeSing stb, SomeSing sxs) ->
        SomeSing $
        SATMap sta stb sxs
  toSing (ATBigMap ta tb xs) =
    case (toSing ta, toSing tb, toSing xs) of
      (SomeSing sta, SomeSing stb, SomeSing sxs) ->
        SomeSing $
        SATBigMap sta stb sxs


-- | Type to tag custom implementation of `EntryPointsDerivation`
data EpdCustom

-- | List is always empty
type family AnnotationEntryPoints (ann :: Annotated Symbol t) :: [(Symbol, Type)] where
  AnnotationEntryPoints _ = '[]

-- | Lookup always fails
type family AnnotationLookupEntryPoint (ann :: Annotated Symbol t) :: Symbol -> Exp (Maybe Type) where
  AnnotationLookupEntryPoint _ = Fcf.ConstFn 'Nothing

-- | This allows EntryPoint annotations to be provided dynamically at run time.
--
-- Note: `EpdAllEntryPoints` is empty and lookup always fails. This just implements `epdNotes`
instance SingI ann => EntryPointsDerivation EpdCustom (AnnotatedParam t ann) where
  type EpdAllEntryPoints EpdCustom (AnnotatedParam t ann) = AnnotationEntryPoints ann
  type EpdLookupEntryPoint EpdCustom (AnnotatedParam t ann) = AnnotationLookupEntryPoint ann
  epdNotes = annotatedToNotes (fromSing (sing @ann))

  epdCall _ = EpConstructionFailed

instance (NiceParameter (Value t), SingI ann) => ParameterHasEntryPoints (AnnotatedParam t ann) where
  type ParameterEntryPointsDerivation (AnnotatedParam t ann) = EpdCustom

-- | A parameter that can be annotated at the type level, allowing
-- `Notes` to be provided as a value at runtime and resolved
-- to a `ParameterHasEntryPoints` instance
data AnnotatedParam (t :: T) (ann :: Annotated Symbol t) where
  AnnotatedParam :: Value t -> AnnotatedParam t ann

instance IsoValue (AnnotatedParam t ann) where
  type ToT (AnnotatedParam t ann) = t
  toVal (AnnotatedParam xs) = xs
  fromVal = AnnotatedParam

