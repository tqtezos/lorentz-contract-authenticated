{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Authenticated where

import Prelude hiding ((>>), drop, swap, get)
import GHC.Generics (Generic(..))
import Text.Show (Show(..))

import Lorentz
import Michelson.Text
import Michelson.Typed.Annotation
import Michelson.Untyped.Annotation

import Lorentz.EntryPoints.Parameter.Choice

import Data.Singletons
import qualified Fcf


data Parameter a
  = WrappedParameter
      { wrappedParameter :: !a
      }
  | SetAdmin
      { admin :: !Address
      }
  | GetAdmin
      { viewAdmin :: !(View () Address)
      }
  deriving  (Generic)

-- | Type to tag custom implementation of `EntryPointsDerivation`
data EpdCustom

instance (SingI (ToT a), Typeable (ToT a)) => EntryPointsDerivation EpdCustom (Parameter a) where
  type EpdAllEntryPoints EpdCustom (Parameter a) = '[ '("wrappedParameter", a), '("setAdmin", Address), '("getAdmin", View () Address)]
  type EpdLookupEntryPoint EpdCustom (Parameter a) = Fcf.Case
    [ "wrappedParameter" Fcf.--> 'Just a
    , "setAdmin" Fcf.--> 'Just Address
    , "getAdmin" Fcf.--> 'Just (View () Address)
    , Fcf.Else (Fcf.ConstFn 'Nothing)
    ]
  epdNotes =
    case starNotes @(ToT (Parameter a)) of
      NTOr ta _ tb as bs ->
        case bs of
          NTOr tc _ _ cs ds ->
            NTOr ta (AnnotationUnsafe "wrappedParameter") tb as $
            NTOr tc (AnnotationUnsafe "setAdmin") (AnnotationUnsafe "getAdmin") cs ds

  epdCall = error "EntryPointsDerivation EpdCustom (Parameter a): epdCall is undefined"

instance (NiceParameter a) => ParameterHasEntryPoints (Parameter a) where
  type ParameterEntryPointsDerivation (Parameter a) = EpdCustom

deriving instance Show a => Show (Parameter a)

deriving instance IsoValue a => IsoValue (Parameter a)

data Storage a =
  Storage
    { wrappedStorage :: !a
    , admin :: !Address
    }
  deriving  (Generic)

deriving instance Show a => Show (Storage a)

deriving instance IsoValue a => IsoValue (Storage a)

-- | Wrap `Storage` with `pair`ing
mkStorage :: a & Address & s :-> Storage a & s
mkStorage = do
  pair
  toStorage

-- | Unwrap `Storage`
unStorage :: Storage a & s :-> (a, Address) & s
unStorage = forcedCoerce_

-- | Wrap `Storage`
toStorage :: (a, Address) & s :-> Storage a & s
toStorage = forcedCoerce_

-- | Assert sender is the given address or fail with an error
assertAdmin_ :: Address & s :-> s
assertAdmin_ = do
  sender
  assertEq $ mkMTextUnsafe "only admin"

-- | `assertAdmin_`, but preserve the stack
assertAdmin :: Address & s :-> Address & s
assertAdmin = do
  dup
  dip assertAdmin_

-- | All entrypoints assigned `True` by `EntryPointChoices`
-- can only be called by the @admin@.
authenticatedContract :: forall cp st. (IsoValue cp)
  => EntryPointChoices cp
  -> Contract cp st
  -> Contract (Parameter cp) (Storage st)
authenticatedContract epChoices wrappedContract = do
  unpair
  caseT @(Parameter cp)
    ( #cWrappedParameter /-> do
        dip $ do
          unStorage
          unpair
          swap
        dup
        runEntryPointChoices epChoices
        if_
          (dip assertAdmin)
          nop
        swap
        dip $ do
          pair
          wrappedContract
          unpair
        swap
        dip $ do
          swap
          pair
          toStorage
        pair
    , #cSetAdmin /-> setAdmin
    , #cGetAdmin /-> getAdmin
    )

-- | Set the admin `Address`
--
-- (Only admin may call)
setAdmin :: forall a. () => Entrypoint Address (Storage a)
setAdmin = do
  dip $ do
    unStorage
    unpair
    swap
    assertAdmin_
  swap
  pair
  toStorage
  nil
  pair

------------------
-- View parameters
------------------

-- | Get the admin `Address` of the contract
getAdmin :: forall a. () => Entrypoint (View () Address) (Storage a)
getAdmin =
  view_ $ do
    cdr
    unStorage
    cdr

