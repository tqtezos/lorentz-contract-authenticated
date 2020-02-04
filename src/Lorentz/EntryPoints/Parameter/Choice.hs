{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-orphans #-}

module Lorentz.EntryPoints.Parameter.Choice where

import Prelude hiding ((>>), drop, swap, get, foldMap)
import GHC.TypeLits
import Data.Foldable

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Lorentz
import Lorentz.EntryPoints.Core
import Michelson.Typed.T
import Michelson.Typed.Sing
import Michelson.Typed.Scope
import Michelson.Typed.Instr
import Michelson.Typed.Value
import Michelson.Typed.EntryPoints

import Data.Singletons
import qualified Data.Text as T

instance IsoValue (Value' Instr t) where
  type ToT (Value' Instr t) = t
  toVal = id
  fromVal = id

instance IsoCValue (Value' Instr ('Tc ct)) where
  type ToCT (Value' Instr ('Tc ct)) = ct
  toCVal (VC xs) = xs
  fromCVal = VC

instance IsoCValue (CValue t) where
  type ToCT (CValue t) = t
  toCVal = id
  fromCVal = id

-- | Given a default choice and a `Map` from entrypoint names to choices, produce an `EntryPointChoices`
chooseEntryPoints :: forall cp. (ParameterScope (ToT cp), ParameterHasEntryPoints cp) => Bool -> Map Text Bool -> EntryPointChoices cp
chooseEntryPoints defaultBool labels = toEntryPointChoices @cp defaultBool $ Map.toAscList labels

-- | Given `ParamNotes`, a default choice, and a `Map` from entrypoint names to choices, produce an `EntryPointChoices`
chooseEntryPointsFromNotes :: forall cp. ParameterScope (ToT cp) => ParamNotes (ToT cp) -> Bool -> Map Text Bool -> EntryPointChoices cp
chooseEntryPointsFromNotes paramNotes' defaultBool labels = toEntryPointChoicesFromNotes @cp paramNotes' defaultBool $ Map.toAscList labels

-- | This is an isomorphism for suitable @cp@ if the function
-- is total and polymorphic in @s@.
runEntryPointChoices :: forall cp s. EntryPointChoices cp -> cp & s :-> Bool & s
runEntryPointChoices (EntryPointChoices xs) = do
  forcedCoerce_ @cp @(Value (ToT cp))
  runEntryPointChoicesT @(ToT cp) xs

-- | If there are one more more values in the list and they're all equal,
-- return that value
maybeOne :: Eq a => [a] -> Maybe a
maybeOne [] = Nothing
maybeOne (x:xs) = loop' xs
  where
    loop' [] = Just x
    loop' (y:ys) =
      case x == y of
        True -> loop' ys
        False -> Nothing

-- | A form of `IF_LEFT` that only accepts `RfNormal` arguments.
--
-- This is useful since the lorentz function produced by `runEntryPointChoices`
-- is recursively `RfNormal`.
rfNormalIfLEFT :: forall a b s. (Value a & s :-> Bool & s) -> (Value b & s :-> Bool & s) -> Value ('TOr a b) & s :-> Bool & s
rfNormalIfLEFT (LorentzInstr (RfNormal f)) (LorentzInstr (RfNormal g)) = LorentzInstr $ RfNormal $ IF_LEFT f g
rfNormalIfLEFT (LorentzInstr (RfAlwaysFails _)) (LorentzInstr (RfAlwaysFails _)) = error "rfNormalIfLEFT: both arguments are RfAlwaysFails"
rfNormalIfLEFT (LorentzInstr (RfAlwaysFails _)) _ = error "rfNormalIfLEFT: left argument is RfAlwaysFails"
rfNormalIfLEFT _ (LorentzInstr (RfAlwaysFails _)) = error "rfNormalIfLEFT: right argument is RfAlwaysFails"

-- | This function implements `ifLeft` pruning: if all leaves of a subtree
-- are all one value, it will return that value instead of recursing.
runEntryPointChoicesT :: forall t s. EntryPointChoicesT t -> Value t & s :-> Bool & s
runEntryPointChoicesT (EntryPointChoicesTOr xs ys) =
  case (mSingleChoiceLeft, mSingleChoiceRight) of
    (Just singleChoiceLeft, Just singleChoiceRight) ->
      case singleChoiceLeft == singleChoiceRight of
        True -> do
           drop
           push singleChoiceLeft
        False -> do
           rfNormalIfLEFT
            (drop >> push singleChoiceLeft)
            (drop >> push singleChoiceRight)
    _ -> do
      rfNormalIfLEFT
        (runEntryPointsChoicesMaybeOne mSingleChoiceLeft xs)
        (runEntryPointsChoicesMaybeOne mSingleChoiceRight ys)
  where
    mSingleChoiceLeft = maybeOne $ entryPointChoicesTToList xs
    mSingleChoiceRight = maybeOne $ entryPointChoicesTToList ys
    runEntryPointsChoicesMaybeOne :: forall t' s'. Maybe Bool -> EntryPointChoicesT t' -> Value t' & s' :-> Bool & s'
    runEntryPointsChoicesMaybeOne mSingleChoice zs = do
      case mSingleChoice of
        Nothing -> runEntryPointChoicesT zs
        Just singleChoice -> do
          drop
          push singleChoice
runEntryPointChoicesT (EntryPointChoicesTOther _ b) = do
  drop
  push b

-- | More general form of `chooseEntryPoints`
toEntryPointChoices :: forall cp t. (ParameterScope (ToT cp), ParameterHasEntryPoints cp, Foldable t) => Bool -> t (Text, Bool) -> EntryPointChoices cp
toEntryPointChoices defaultBool = flip appEndo (emptyEntryPointChoices defaultBool) . foldMap (\(name, newChoice) -> Endo $
  case someSymbolVal $ T.unpack name of
    SomeSymbol (_ :: Proxy name') ->
      setEntryPointChoice @cp (Label @name') newChoice
  )

-- | More general form of `chooseEntryPointsFromNotes`
toEntryPointChoicesFromNotes :: forall cp t. (ParameterScope (ToT cp), Foldable t) => ParamNotes (ToT cp) -> Bool -> t (Text, Bool) -> EntryPointChoices cp
toEntryPointChoicesFromNotes paramNotes' defaultBool = flip appEndo (emptyEntryPointChoices defaultBool) . foldMap (\(name, newChoice) -> Endo $
  case someSymbolVal $ T.unpack name of
    SomeSymbol (_ :: Proxy name') ->
      setEntryPointChoiceFromNotes @cp paramNotes' (Label @name') newChoice
  )

-- | Set a particular entrypoint choice given its label
setEntryPointChoice :: forall cp name. (ParameterScope (ToT cp), ParameterHasEntryPoints cp, KnownSymbol name) => Label name -> Bool -> EntryPointChoices cp -> EntryPointChoices cp
setEntryPointChoice label newChoice (EntryPointChoices epChoices) =
  case epdCall @(ParameterEntryPointsDerivation cp) @cp label of
    EpConstructionFailed -> error $ unwords ["Entrypoint resolution failed for:", Prelude.show (sing @name)]
    EpConstructed epLiftSequence -> EntryPointChoices $
      setEntryPointChoiceT newChoice epChoices epLiftSequence

-- | Set a particular entrypoint choice given its label and `ParamNotes`
setEntryPointChoiceFromNotes :: forall cp name. (ParameterScope (ToT cp), KnownSymbol name) => ParamNotes (ToT cp) -> Label name -> Bool -> EntryPointChoices cp -> EntryPointChoices cp
setEntryPointChoiceFromNotes paramNotes' _ newChoice (EntryPointChoices epChoices) =
  case mkEntryPointCall @(ToT cp) (EpNameUnsafe . fromSing $ sing @name) (sing @(ToT cp), paramNotes') of
    Nothing -> error $ unwords ["Entrypoint resolution failed for:", Prelude.show (sing @name)]
    Just (MkEntryPointCallRes _ epCallT) ->
      case epCallT of
        EntryPointCall _ _ epLiftSequence -> EntryPointChoices $
          setEntryPointChoiceT newChoice epChoices epLiftSequence

-- | Set an entrypoint choice given an `EpLiftSequence`
setEntryPointChoiceT :: forall arg cp. ()
  => Bool
  -> EntryPointChoicesT cp
  -> EpLiftSequence arg cp
  -> EntryPointChoicesT cp
setEntryPointChoiceT newChoice xs EplArgHere = setDefaultEntryPointChoiceT newChoice xs
setEntryPointChoiceT newChoice epChoices (EplWrapLeft  epLiftSequence) =
  case epChoices of
    (EntryPointChoicesTOr xs ys) -> EntryPointChoicesTOr (setEntryPointChoiceT newChoice xs epLiftSequence) ys
    (EntryPointChoicesTOther (NotOrTOr prfNotOr) _) -> absurd prfNotOr
setEntryPointChoiceT newChoice epChoices (EplWrapRight epLiftSequence) =
  case epChoices of
    (EntryPointChoicesTOr xs ys) -> EntryPointChoicesTOr xs (setEntryPointChoiceT newChoice ys epLiftSequence)
    (EntryPointChoicesTOther (NotOrTOr prfNotOr) _) -> absurd prfNotOr

-- | Set all choices to the given one
setDefaultEntryPointChoiceT :: forall cp. ()
  => Bool
  -> EntryPointChoicesT cp
  -> EntryPointChoicesT cp
setDefaultEntryPointChoiceT newChoice (EntryPointChoicesTOr xs ys) =
  EntryPointChoicesTOr
    (setDefaultEntryPointChoiceT newChoice xs)
    (setDefaultEntryPointChoiceT newChoice ys)
setDefaultEntryPointChoiceT newChoice (EntryPointChoicesTOther prfNotOr _) =
  EntryPointChoicesTOther prfNotOr newChoice

-- | All choices are given default
emptyEntryPointChoices :: forall cp. SingI (ToT cp) => Bool -> EntryPointChoices cp
emptyEntryPointChoices = EntryPointChoices @cp . emptyEntryPointChoicesT @(ToT cp) (sing @(ToT cp))

-- | All choices are given default
emptyEntryPointChoicesT :: forall t. Sing t -> Bool -> EntryPointChoicesT t
emptyEntryPointChoicesT st defaultBool =
  case st of
    (STc ct) -> EntryPointChoicesTOther (NotOrTc ct) defaultBool
    (STKey) -> EntryPointChoicesTOther (NotOrTKey) defaultBool
    (STUnit) -> EntryPointChoicesTOther (NotOrTUnit) defaultBool
    (STSignature) -> EntryPointChoicesTOther (NotOrTSignature) defaultBool
    (STChainId) -> EntryPointChoicesTOther (NotOrTChainId) defaultBool
    (STOption a) -> EntryPointChoicesTOther (NotOrTOption a) defaultBool
    (STList a) -> EntryPointChoicesTOther (NotOrTList a) defaultBool
    (STSet ct) -> EntryPointChoicesTOther (NotOrTSet ct) defaultBool
    (STOperation) -> EntryPointChoicesTOther (NotOrTOperation) defaultBool
    (STContract a) -> EntryPointChoicesTOther (NotOrTContract a) defaultBool
    (STPair a b) -> EntryPointChoicesTOther (NotOrTPair a b) defaultBool
    (STOr a b) -> EntryPointChoicesTOr (emptyEntryPointChoicesT a defaultBool) (emptyEntryPointChoicesT b defaultBool)
    (STLambda a b) -> EntryPointChoicesTOther (NotOrTLambda a b) defaultBool
    (STMap ct a) -> EntryPointChoicesTOther (NotOrTMap ct a) defaultBool
    (STBigMap ct a) -> EntryPointChoicesTOther (NotOrTBigMap ct a) defaultBool

-- | EntryPointChoices annotates all top-level sums with `Bool`s
data EntryPointChoices a where
  EntryPointChoices :: EntryPointChoicesT (ToT a) -> EntryPointChoices a

-- | Either we have a top-level sum and recurse or we're at a leaf and
-- provide a `NotOr` proof and a `Bool`
data EntryPointChoicesT (t :: T) where
  EntryPointChoicesTOr :: forall a b. EntryPointChoicesT a -> EntryPointChoicesT b -> EntryPointChoicesT ('TOr a b)
  EntryPointChoicesTOther :: forall t. NotOr t -> Bool -> EntryPointChoicesT t

-- | A list of all `Bool`'s in a `EntryPointChoicesT`
--
-- Used to implement the optimization in `runEntryPointChoicesT`
entryPointChoicesTToList :: EntryPointChoicesT t -> [Bool]
entryPointChoicesTToList (EntryPointChoicesTOr xs ys) = entryPointChoicesTToList xs ++ entryPointChoicesTToList ys
entryPointChoicesTToList (EntryPointChoicesTOther _ b) = [b]

-- | A Proof that a given `T` is not @`TOr` _ _@:
-- otherwise the contstructor contains `Void`
data NotOr (t :: T) where
  NotOrTc :: forall ct. Sing ct -> NotOr ('Tc ct)
  NotOrTKey :: NotOr ('TKey)
  NotOrTUnit :: NotOr ('TUnit)
  NotOrTSignature :: NotOr ('TSignature)
  NotOrTChainId :: NotOr ('TChainId)
  NotOrTOption :: forall a. Sing a -> NotOr ('TOption a)
  NotOrTList :: forall a. Sing a -> NotOr ('TList a)
  NotOrTSet :: forall ct. Sing ct -> NotOr ('TSet ct)
  NotOrTOperation :: NotOr ('TOperation)
  NotOrTContract :: forall a. Sing a -> NotOr ('TContract a)
  NotOrTPair :: forall a b. Sing a -> Sing b -> NotOr ('TPair a b)
  NotOrTOr :: forall a b. Void -> NotOr ('TOr a b)
  NotOrTLambda :: forall a b. Sing a -> Sing b -> NotOr ('TLambda a b)
  NotOrTMap :: forall ct a. Sing ct -> Sing a -> NotOr ('TMap ct a)
  NotOrTBigMap :: forall ct a. Sing ct -> Sing a -> NotOr ('TBigMap ct a)

