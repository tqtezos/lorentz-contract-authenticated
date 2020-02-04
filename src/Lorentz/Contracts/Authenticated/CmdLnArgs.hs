{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Authenticated.CmdLnArgs where

import Control.Applicative
import Control.Monad (Monad(..), liftM2)
import Text.Show (Show(..))
import Data.List
import Data.Char
import Data.Either
import Data.Function (id, const)
import Data.Functor
import Data.Foldable
import Prelude (FilePath, IO, uncurry, runReaderT, flip)
import Data.String (String) -- IsString(..), String)
import Data.Maybe
import Data.Typeable
import Text.Read
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P

import Lorentz hiding (get)
import Michelson.Parser
import Michelson.Typed.Annotation
-- import Michelson.Typed.Arith
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Scope
import Michelson.Typed.EntryPoints
import Michelson.Typed.Instr
import Michelson.Typed.Sing
import Michelson.Typed.T
-- import Michelson.Typed.Value
import Util.IO
import qualified Michelson.Untyped.Type as U
import Michelson.Macro
import Michelson.TypeCheck.Instr
import Michelson.TypeCheck.TypeCheck
import qualified Tezos.Address as Tezos
import qualified Michelson.TypeCheck.Types as TypeCheck

import qualified Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Constraint hiding (contract)
import Data.Singletons
import Text.Megaparsec (parse, eof)

-- import Lorentz.Contracts.Util ()
-- import Lorentz.Contracts.SomeContractParam
-- import Lorentz.Contracts.Parse
-- import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G

import qualified Lorentz.Contracts.Authenticated as Authenticated
import Lorentz.EntryPoints.Parameter.Choice

-- Read Address instance

-- instance ParameterHasEntryPoints cp => ParameterHasEntryPoints (Value cp) where
--   type ParameterEntryPointsDerivation (Value cp) = ParameterEntryPointsDerivation cp


-- | Parse something between the two given `Char`'s
betweenChars :: Char -> Char -> ReadP a -> ReadP a
betweenChars beforeChar afterChar =
  P.char beforeChar `P.between` P.char afterChar

-- | Parse something in parentheses
inParensP :: ReadP a -> ReadP a
inParensP = '(' `betweenChars` ')'

-- | Parse something in double-quotes: @"[something]"@
inQuotesP :: ReadP a -> ReadP a
inQuotesP = '"' `betweenChars` '"'

-- | Attempt to parse with given modifier, otherwise parse without
maybeLiftP :: (ReadP a -> ReadP a) -> ReadP a -> ReadP a
maybeLiftP liftP = liftM2 (<|>) liftP id

-- | Attempt to parse `inParensP`, else parse without
maybeInParensP :: ReadP a -> ReadP a
maybeInParensP = maybeLiftP inParensP

-- | Attempt to parse `inQuotesP`, else parse without
maybeInQuotesP :: ReadP a -> ReadP a
maybeInQuotesP = maybeLiftP inQuotesP

-- | Read an `Address`, inside or outside of @""@'s
readAddressP :: ReadP Address
readAddressP =
      maybeInParensP . maybeInQuotesP $ do
        ensureAddressPrefix
        addressStr <- P.munch1 isAlphaNum
        case Tezos.parseAddress $ T.pack addressStr of
          Left err -> fail $ show err
          Right address' -> return address'
  where
    ensureAddressPrefix =
      (do {('t':'z':'1':_) <- P.look; return ()}) <|>
      (do {('K':'T':'1':_) <- P.look; return ()})

instance Read Address where
  readPrec = readP_to_Prec $ const readAddressP

-- End Read Address instance

-- | Parse whether to output on one line
onelineOption :: Opt.Parser Bool
onelineOption = Opt.switch (
  Opt.long "oneline" <>
  Opt.help "Force single line output")

-- | Parse the output `FilePath`
outputOptions :: Opt.Parser (Maybe FilePath)
outputOptions = optional . Opt.strOption $ mconcat
  [ Opt.short 'o'
  , Opt.long "output"
  , Opt.metavar "FILEPATH"
  , Opt.help "File to use as output. If not specified, stdout is used."
  ]

-- | Parse an `Address` argument, given its field name
parseAddress :: String -> Opt.Parser Address
parseAddress name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "ADDRESS"
    , Opt.help $ "Address of the " ++ name ++ "."
    ]

-- | Parse a `Bool` (optional) argument, given its field name
parseBool :: String -> Opt.Parser Bool
parseBool name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "BOOL"
    , Opt.help $
      "Bool representing whether the contract is initially " ++ name ++ "."
    ]

-- | Parse a `View` by parsing its arguments and @"callback-contract"@ address
parseView :: NiceParameter r => Opt.Parser a -> Opt.Parser (View a r)
parseView parseArg =
  View <$> parseArg <*> fmap toContractRef (parseAddress "callback-contract")




-- | Parse and typecheck a Michelson value
parseTypeCheckValue ::
     forall t. (Typeable t, SingI t)
  => Parser (Value t)
parseTypeCheckValue =
  (>>= either (fail . show) return) $
  runTypeCheckIsolated . flip runReaderT def . typeVerifyValue . expandValue <$>
  (value <* eof)


assertOpAbsense :: forall (t :: T) a. SingI t => (HasNoOp t => a) -> a
assertOpAbsense f =
  case opAbsense (sing @t) of
    Nothing -> error "assertOpAbsense"
    Just Dict -> forbiddenOp @t f

assertContractAbsense :: forall (t :: T) a. SingI t => (HasNoContract t => a) -> a
assertContractAbsense f =
  case contractTypeAbsense (sing @t) of
    Nothing -> error "assertContractAbsense"
    Just Dict -> forbiddenContractType @t f

assertBigMapAbsense :: forall (t :: T) a. SingI t => (HasNoBigMap t => a) -> a
assertBigMapAbsense f =
  case bigMapAbsense (sing @t) of
    Nothing -> error "assertBigMapAbsense"
    Just Dict -> forbiddenBigMap @t f

assertNestedBigMapsAbsense :: forall (t :: T) a. SingI t => (HasNoNestedBigMaps t => a) -> a
assertNestedBigMapsAbsense f =
  case nestedBigMapsAbsense (sing @t) of
    Nothing -> error "assertNestedBigMapsAbsense"
    Just Dict -> forbiddenNestedBigMaps @t f

-- type IsComparable c = ToT c ~ 'Tc (ToCT c)
assertIsComparable ::
     forall (t :: T) a. SingI t
  => (( IsComparable (Value t)
      , SingI (ToCT (Value t))
      , Typeable (ToCT (Value t))
      ) =>
        a)
  -> a
assertIsComparable f =
  case sing @t of
    STc _ -> f
    _ -> error "assertIsComparable"

singTypeableCT :: forall (t :: CT). Sing t -> Dict (Typeable t)
singTypeableCT SCInt = Dict
singTypeableCT SCNat = Dict
singTypeableCT SCString = Dict
singTypeableCT SCBytes = Dict
singTypeableCT SCMutez = Dict
singTypeableCT SCBool = Dict
singTypeableCT SCKeyHash = Dict
singTypeableCT SCTimestamp = Dict
singTypeableCT SCAddress = Dict

singTypeableT :: forall (t :: T). Sing t -> Dict (Typeable t)
singTypeableT (STc ct) =
  withDict (singTypeableCT ct) $
  Dict
singTypeableT STKey = Dict
singTypeableT STUnit = Dict
singTypeableT STSignature = Dict
singTypeableT STChainId = Dict
singTypeableT (STOption st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STList st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STSet st) =
  withDict (singTypeableCT st) $
  Dict
singTypeableT STOperation  = Dict
singTypeableT (STContract st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STPair st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STOr st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STLambda st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STBigMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict

singICT :: forall (t :: CT). Sing t -> Dict (SingI t)
singICT SCInt = Dict
singICT SCNat = Dict
singICT SCString = Dict
singICT SCBytes = Dict
singICT SCMutez = Dict
singICT SCBool = Dict
singICT SCKeyHash = Dict
singICT SCTimestamp = Dict
singICT SCAddress = Dict

singIT :: forall (t :: T). Sing t -> Dict (SingI t)
singIT (STc ct) =
  withDict (singICT ct) $
  Dict
singIT STKey = Dict
singIT STUnit = Dict
singIT STSignature = Dict
singIT STChainId = Dict
singIT (STOption st) =
  withDict (singIT st) $
  Dict
singIT (STList st) =
  withDict (singIT st) $
  Dict
singIT (STSet st) =
  withDict (singICT st) $
  Dict
singIT STOperation  = Dict
singIT (STContract st) =
  withDict (singIT st) $
  Dict
singIT (STPair st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STOr st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STLambda st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict
singIT (STBigMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict

data SomeContractStorage where
  SomeContractStorage :: (NiceStorage (Value a))
    => Value a
    -> SomeContractStorage

fromSomeContractStorage :: forall b. SomeContractStorage -> (forall a. NiceStorage (Value a) => Value a -> b) -> b
fromSomeContractStorage (SomeContractStorage xs) f = f xs

-- | A contract parameter with some type
data SomeContractParam where
  SomeContractParam
    :: (SingI t, Typeable t)
    => Value t
    -> (Sing t, Notes t)
    -> (Dict (HasNoOp t), Dict (HasNoBigMap t))
    -> SomeContractParam


data CmdLnArgs
  = Print
      { defaultAuthenticated :: Bool
      , authenticatedEntrypoints :: Map Text Bool
      , wrappedContract :: TypeCheck.SomeContract
      , mOutput :: Maybe FilePath
      , forceSingleLine :: Bool
      }
  | Init
      { initialWrappedStorage :: !SomeContractStorage
      , admin :: !Address
      }
  | SetAdmin
      { admin :: !Address
      }
  | GetAdmin
      { viewAdmin :: !(View () Address)
      }
  | WrappedParam
      { wrappedParam :: !SomeContractParam
      }

-- | Make a type non-explicit
unExplicitType :: U.Type -> U.T
unExplicitType =
  \case
    U.Type t _ -> t

-- | Convert a `U.Comparable` to `CT`
fromUntypedComparable :: U.Comparable -> CT
fromUntypedComparable (U.Comparable ct _) = ct

-- | Convert a `U.Type` to `T`
fromUntypedT' :: U.Type -> T
fromUntypedT' = fromUntypedT . unExplicitType

-- | Convert a `U.T` to `T`
fromUntypedT :: U.T -> T
fromUntypedT (U.Tc ct) = Tc ct
fromUntypedT U.TKey = TKey
fromUntypedT U.TUnit = TUnit
fromUntypedT U.TChainId = TChainId
fromUntypedT U.TSignature = TSignature
fromUntypedT (U.TOption x) = TOption $ fromUntypedT' x
fromUntypedT (U.TList x) = TList $ fromUntypedT' x
fromUntypedT (U.TSet ct) = TSet $ fromUntypedComparable ct
fromUntypedT U.TOperation = TOperation
fromUntypedT (U.TContract x) = TContract $ fromUntypedT' x
fromUntypedT (U.TPair _ _ x y) = TPair (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TOr _ _ x y) = TOr (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TLambda x y) = TLambda (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TMap ct x) = TMap (fromUntypedComparable ct) $ fromUntypedT' x
fromUntypedT (U.TBigMap ct x) = TBigMap (fromUntypedComparable ct) $ fromUntypedT' x

-- | Parse some `T`
parseSomeT :: String -> Opt.Parser (SomeSing T)
parseSomeT name =
  (\typeStr ->
    let parsedType = parseNoEnv
          type_
          name
          typeStr
     in let type' = either (error . T.pack . show) unExplicitType parsedType
     in withSomeSingT (fromUntypedT type') SomeSing
  ) <$>
  Opt.strOption @Text
    (mconcat
      [ Opt.long $ name ++ "Type"
      , Opt.metavar "Michelson Type"
      , Opt.help $ "The Michelson Type of " ++ name
      ])

parseSomeContract :: String -> Opt.Parser TypeCheck.SomeContract
parseSomeContract name =
  Opt.option (Opt.str >>= someContractParser)
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Contract Source"
      , Opt.help $ "The Michelson contract: " ++ name
      ])
  where
  someContractParser :: Text -> Opt.ReadM TypeCheck.SomeContract
  someContractParser = either (fail . show) (either (fail . show) return . typeCheckContract mempty . expandContract) . parse program name

parseSomeContractParam :: String -> Opt.Parser SomeContractParam
parseSomeContractParam name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let parsedParam = parseNoEnv
          (parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractParam param (st, starNotes) (Dict, Dict)
  ) <$>
  parseSomeT name <*>
  Opt.strOption @Text
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Value"
      , Opt.help $ "The Michelson Value: " ++ name
      ])

parseMaybe :: Alternative f => f a -> f (Maybe a)
parseMaybe p = fmap Just p <|> pure Nothing

parseSomeContractStorage :: String -> Opt.Parser SomeContractStorage
parseSomeContractStorage name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertContractAbsense @t $
    assertBigMapAbsense @t $
    assertNestedBigMapsAbsense @t $
    let parsedParam = parseNoEnv
          (parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractStorage param
  ) <$>
  parseSomeT name <*>
  Opt.strOption @Text
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Value"
      , Opt.help $ "The Michelson Value: " ++ name
      ])

parseView_ :: NiceParameter r => Opt.Parser (View () r)
parseView_ = parseView $ pure ()

assertUniqToMap :: [(Text, Bool)] -> Opt.ReadM (Map Text Bool)
assertUniqToMap = foldrM (uncurry folder) mempty
  where
    folder :: Text -> Bool -> Map Text Bool -> Opt.ReadM (Map Text Bool)
    folder k v = flip Map.alterF k $ \mvPrev ->
      case mvPrev of
        Nothing -> return $ Just v
        Just vPrev -> fail $ unwords ["Duplicate values:", show k, show vPrev, show v]

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , initSubCmd
  , setAdminSubCmd
  , getAdminSubCmd
  , wrappedParamSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSubCmd =
      mkCommandParser "print"
      (Print <$>
        parseBool "default-authenticated" <*>
        (Opt.option (Opt.auto >>= assertUniqToMap) $ mconcat
          [ Opt.long "authenticated-entrypoints"
          , Opt.metavar "[(ENTRYPOINT_STRING, REQUIRES_ADMIN_BOOL)]"
          , Opt.help $ "String representing the contract's authenticated entrypoints."
          ]) <*>
        parseSomeContract "wrapped" <*>
        outputOptions <*>
        onelineOption
      )
      "Dump the Whitelist contract in form of Michelson code"

    initSubCmd =
      mkCommandParser "init"
      (Init <$>
        parseSomeContractStorage "initial-wrapped" <*>
        parseAddress "admin"
      )
      ("Initial storage for the (wrapped) Whitelist contract: " <>
      "pass 'initialWrappedStorage' for the wrapped version")

    setAdminSubCmd =
      mkCommandParser "SetAdmin"
      (SetAdmin <$> parseAddress "admin")
      "Generate the (wrapped) parameter for the Whitelist contract: SetAdmin"

    getAdminSubCmd =
      mkCommandParser "GetAdmin"
      (GetAdmin <$> parseView_)
      "Generate the (wrapped) parameter for the Whitelist contract: GetAdmin"

    wrappedParamSubCmd =
      mkCommandParser "WrappedParam"
      (WrappedParam <$> parseSomeContractParam "wrappedParam")
      ("Generate a wrapped parameter for the Whitelist contract, given the " <>
      "original contract's parameter")

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Whitelist contract CLI interface"
  ]

-- data CmdLnArgs
--   = Print
--       { defaultAuthenticated :: Bool
--       , authenticatedEntrypoints :: Map Text Bool
--       , wrappedContract :: TypeCheck.SomeContract
--       , mOutput :: Maybe FilePath
--       , forceSingleLine :: Bool
--       }
--   | Init
--       { initialWrappedStorage :: !SomeContractStorage
--       , admin :: !Address
--       }
--   | SetAdmin
--       { admin :: !Address
--       }
--   | GetAdmin
--       { viewAdmin :: !(View () Address)
--       }
--   | WrappedParam
--       { wrappedParam :: !SomeContractParam
--       }

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  Print {..} -> -- defaultAuthenticated' authenticatedEntrypoints' (TypeCheck.SomeContract xs) mOutput' forceSingleLine' ->
    case wrappedContract of
      TypeCheck.SomeContract wrappedContractFC ->
        case wrappedContractFC of
          FullContract wrappedContractCode (paramNotes' :: ParamNotes cp) (_ :: Notes st) ->
            maybe TL.putStrLn writeFileUtf8 mOutput $
            printLorentzContract forceSingleLine $
            Authenticated.authenticatedContract @(Value cp) @(Value st)
              (chooseEntryPointsFromNotes @(Value cp) paramNotes' defaultAuthenticated authenticatedEntrypoints)
              (I wrappedContractCode)
  Init (SomeContractStorage (initialWrappedStorage :: Value st)) admin ->
    TL.putStrLn $
    printLorentzValue @(Authenticated.Storage (Value st)) forceOneLine $
    Authenticated.Storage initialWrappedStorage admin
  SetAdmin {..} ->
    TL.putStrLn $
    printLorentzValue @(Authenticated.Parameter ()) forceOneLine $
    Authenticated.SetAdmin admin
  GetAdmin {..} ->
    TL.putStrLn $
    printLorentzValue @(Authenticated.Parameter ()) forceOneLine $
    Authenticated.GetAdmin viewAdmin
  WrappedParam (SomeContractParam (wrappedParam' :: Value cp) _ (Dict, Dict)) ->
    TL.putStrLn $
    printLorentzValue @(Authenticated.Parameter (Value cp)) forceOneLine $
    Authenticated.WrappedParameter wrappedParam'
  where
    forceOneLine = True

-- data SomeContractParam where
--   SomeContractParam
--     :: (SingI t, Typeable t)
--     => Value t
--     -> (Sing t, Notes t)
--     -> (Dict (HasNoOp t), Dict (HasNoBigMap t))
--     -> SomeContractParam


  -- Print (SomeSing (st :: Sing t)) mOutput forceOneLine ->
  --   withDict (singIT st) $
  --   withDict (singTypeableT st) $
  --   assertOpAbsense @t $
  --   assertBigMapAbsense @t $
  --   assertIsComparable @t $
  --   withDict (compareOpCT @(ToCT (Value t))) $
  --   maybe TL.putStrLn writeFileUtf8 mOutput $
  --   printLorentzContract forceOneLine (Whitelist.whitelistContract @(Value t))
  -- Init {..} ->
  --   fromSomeStorage initialStorage $ \(initialStorage' :: Whitelist.Storage (Value s)) ->
  --     assertIsComparable @s $
  --     case initialWrappedStorage of
  --       Nothing ->
  --         TL.putStrLn . printLorentzValue @(Whitelist.Storage (Value s)) forceSingleLine $
  --         initialStorage'
  --       Just initialWrappedStorage' ->
  --         fromSomeContractStorage initialWrappedStorage' $ \(initialWrappedStorage'' :: Value t) ->
  --         let st = sing @t in
  --         withDict (singIT st) $
  --         withDict (singTypeableT st) $
  --         TL.putStrLn $
  --         printLorentzValue @(Wrapper.Storage (Value t) (Value s)) forceSingleLine $
  --         Wrapper.Storage
  --           initialWrappedStorage''
  --           initialStorage'
  -- SetAdmin {..} ->
  --   if wrapped
  --      then
  --        TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
  --        Wrapper.WhitelistParameter $
  --        Whitelist.SetAdmin admin
  --      else
  --        TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
  --        Whitelist.OtherParameter $
  --        Whitelist.SetAdmin admin
  -- GetAdmin {..} ->
  --   if wrapped
  --      then
  --        TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
  --        Wrapper.WhitelistParameter $
  --        Whitelist.GetAdmin viewAdmin
  --      else
  --        TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
  --        Whitelist.OtherParameter $
  --        Whitelist.GetAdmin viewAdmin
  -- WrappedParam {..} ->
  --   fromSomeContractParam wrappedParam $ \(wrappedParam' :: Value t) ->
  --     let st = sing @t in
  --     withDict (singIT st) $
  --     withDict (singTypeableT st) $
  --     TL.putStrLn . printLorentzValue @(Wrapper.Parameter (Value t) ()) forceSingleLine $
  --     Wrapper.WrappedParameter wrappedParam'
  -- where
  --   forceSingleLine = True

