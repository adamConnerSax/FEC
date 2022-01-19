--{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module OpenFEC.Types
  (
    module OpenFEC.Types
  , module OpenFEC.Beam.Types
  ) where

import           Control.Lens
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (join, sequence)
import qualified Data.Aeson                as A
import           Data.Aeson.Lens
import qualified Data.Aeson.Types          as A
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as BS
import           Data.Data                 (Data)
import           Data.Foldable             (foldl')
import qualified Data.Foldable             as F
import qualified Data.List                 as L
import           Data.Scientific           (FPFormat (Fixed), Scientific,
                                            formatScientific)
import           Data.Text                 (Text, pack, unpack)
import           Data.Time.Calendar        (Day)
import           Data.Time.Clock           (UTCTime, utctDay)
import           Data.Time.Format          (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime       (LocalTime(..), TimeOfDay(..), utc, utcToLocalTime, midnight)
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import           GHC.Generics              (Generic)
import qualified Text.PrettyPrint.Tabulate as PP
import           Text.Printf               (printf)
import           Text.Read                 (readMaybe)
import           Web.HttpApiData           (ToHttpApiData (..))


import           OpenFEC.JsonUtils
import qualified OpenFEC.Pagination        as FEC

import           GHC.Int (Int32)
import           OpenFEC.Beam.Types
-- All fields are prefixed with entity name followed by field name.
-- If the field name begins with entity name (e.g., "candidate_id" in Candidate) we begin with double underscore.
-- So, field name to json key is straightforward

--type CandidateID = Text
--type CommitteeID = Text
--type Name = Text
--type State = Text
--type District = Int
--type Amount = Scientific
type ElectionYear = Int32
type ElectionCycle = Int32


officeToText :: Office -> Text
officeToText House     = "H"
officeToText Senate    = "S"
officeToText President = "P"

instance A.ToJSON Office where
  toJSON = A.String . officeToText

instance A.FromJSON Office where
  parseJSON o = A.withText "Office" f o where
    f t = case t of
      "H" -> return House
      "S" -> return Senate
      "P" -> return President
      _   -> A.typeMismatch "Office" o

partyToText :: Party -> Text
partyToText Democrat        = "DEM"
partyToText Republican      = "REP"
partyToText Independent     = "IND"
partyToText WorkingFamilies = "WFP"
partyToText Conservative    = "CON"
partyToText Green           = "GRE"
partyToText Libertarian     = "LIB"
partyToText Other           = "OTH"
partyToText Unknown         = "UNK"

parseParty :: Text -> Maybe Party
parseParty t = case t of
  "DEM" -> Just Democrat
  "REP" -> Just Republican
  "IND" -> Just Independent
  "WFP" -> Just WorkingFamilies
  "CON" -> Just Conservative
  "GRE" -> Just Green
  "LIB" -> Just Libertarian
  "OTH" -> Just Other
  "UNK" -> Just Unknown
  _     -> Just Other -- Nothing

resultToMaybe (A.Error _)   = Nothing
resultToMaybe (A.Success a) = Just a

_Party :: Prism' A.Value Party
_Party = prism' (A.String . partyToText) (join . resultToMaybe . fmap parseParty . A.fromJSON)

instance A.ToJSON Party where
  toJSON = A.String . partyToText

-- this one is used in Parsing from FEC data.  Maybe we can build a custom Prism? "Prism' Text Party" ?
instance A.FromJSON Party where
  parseJSON o = A.withText "Party" f o where
    f = maybe (A.typeMismatch "Party" o) return . parseParty
{-    f t = case t of
      "DEM" -> return Democrat
      "REP" -> return Republican
      "IND" -> return Independent
      "WFP" -> return WorkingFamilies
      "CON" -> return Conservative
      "GRE" -> return Green
      "LIB" -> return Libertarian
      "OTH" -> return Other
      "UNK" -> return Unknown
      _     -> return Other --A.typeMismatch "Party" o
-}

instance A.FromJSON Candidate where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON Candidate where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}


--makeLenses ''Candidate

candidateFromResultJSON :: A.Value -> Either ByteString Candidate
candidateFromResultJSON val = Candidate
  <$> val |#| "candidate_id"
  <*> val |#| "name"
  <*> val |#| "office"
  <*> val |#| "state"
  <*> val |#| "district_number"
  <*> val |#| "party"


instance A.FromJSON Committee where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON Committee where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

candidateTotalsFromResultJSON :: A.Value -> Either ByteString CandidateTotals
candidateTotalsFromResultJSON val = CandidateTotals
  <$> val |#| "candidate_id"
  <*> val |#| "cash_on_hand_end_period"
  <*> val |#| "disbursements"
  <*> val |#| "receipts"
  <*> val |#| "debts_owed_by_committee"
  <*> fmap (\x -> LocalTime x (TimeOfDay 23 59 59)) (tryTwoKeys "last_file_date" "load_date" utctDay val)


instance A.FromJSON CandidateTotals where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON CandidateTotals where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

--makeLenses ''Committee

committeeFromResultJSON :: A.Value -> Either ByteString Committee
committeeFromResultJSON val = Committee
  <$> val |#| "committee_id"
  <*> val |#| "designation_full"
  <*> val |#| "name"
  <*> val |#| "committee_type_full"
  <*> val |#| "committee_type"

committeeWithCandidatesFromResultJSON :: A.Value -> Either ByteString (Committee, [CandidateID])
committeeWithCandidatesFromResultJSON val = (,)
  <$> committeeFromResultJSON val
  <*> val |#| "candidate_ids"

amountToText :: Amount -> Text
amountToText = pack . printf "%.2f"

dayToText :: Day -> Text
dayToText = pack . formatTime defaultTimeLocale "%F"

localTimeToText :: LocalTime -> Text
localTimeToText = pack . formatTime defaultTimeLocale "%F"

utcTimeToText :: UTCTime -> Text
utcTimeToText = pack . formatTime defaultTimeLocale "%F"

data Report = Report
  {
    _report_total_receipts_period         :: Amount
  , _report_total_receipts_ytd            :: Amount
  , _report_total_contributions_period    :: Amount
  , _report_total_contributions_ytd       :: Amount
  , _report_total_disbursements_period    :: Amount
  , _report_total_disbursements_ytd       :: Amount
  , _report_cash_on_hand_beginning_period :: Amount
  , _report_receipt_date                  :: LocalTime
  } deriving (Generic, Show)

instance A.FromJSON Report where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON Report where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

--makeLenses ''Report

reportFromResultJSON :: A.Value -> Either ByteString Report
reportFromResultJSON val = Report
  <$> val |#| "total_receipts_period"
  <*> val |#| "total_receipts_ytd"
  <*> val |#| "total_contributions_period"
  <*> val |#| "total_contributions_ytd"
  <*> val |#| "total_disbursements_period"
  <*> val |#| "total_disbursements_ytd"
  <*> val |#| "cash_on_hand_beginning_period"
  <*> val |#| "receipt_date"


instance A.FromJSON Disbursement where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON Disbursement where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

--makeLenses ''Disbursement
sub_idFromText :: Either ByteString Text -> Either ByteString Int
sub_idFromText t =
  let f = either Left (maybe (Left "Error converting sub_id Text to Int") Right)
  in f $ readMaybe . unpack <$> t

maybeToEitherF :: c -> (a -> Maybe b) -> (a -> Either c b)
maybeToEitherF leftVal f a = case f a of
  Nothing -> Left leftVal
  Just x  -> Right x

disbursementFromResultJSON :: CandidateID -> A.Value -> Either ByteString Disbursement
disbursementFromResultJSON cid val =
  let candidatesValVecE :: Either ByteString (Vector A.Value) = tryP "committee.candidate_ids" (key "committee" . key "candidate_ids" . _Array) val
      candidatesVecE :: Either ByteString (Vector Text) = candidatesValVecE >>= maybeToEitherF "Failed to parse json in array to candidate id" (sequence . fmap (\x -> x ^? _String))
      num_candidatesE :: Either ByteString Int = L.length <$> candidatesVecE
      amountE = val |#| "disbursement_amount"
      adj_amountE = case candidatesVecE of
        Left err -> Left err
        Right candsV -> if cid `V.elem` candsV then (fmap (/(fromIntegral $ V.length candsV)) amountE) else Right 0
  in Disbursement
     <$> tryTwoKeys "disbursement_date" "load_date" (utcToLocalTime utc) val -- val |#| "disbursement_date"
     <*> amountE
     <*> adj_amountE
     <*> fmap fromIntegral num_candidatesE
     <*> val |#| "disbursement_purpose_category"
     <*> val |#| "recipient_name"
     <*> pure (CandidateKey cid)
     <*> val |#| "committee_id"
     <*> val |#| "line_number_label"
     <*> fmap fromIntegral (sub_idFromText (val |#| "sub_id"))
     <*> pure 0

instance PP.CellValueFormatter SpendingIntention

instance A.ToJSON SpendingIntention where
  toJSON Support = A.String "S"
  toJSON Oppose  = A.String "O"

instance A.FromJSON SpendingIntention where
  parseJSON o = A.withText "SpendingIntention" f o where
    f t = case t of
      "S" -> return Support
      "O" -> return Oppose
      _   -> A.typeMismatch "SpendingIntention" o


instance PP.CellValueFormatter LocalTime
instance PP.CellValueFormatter Scientific
instance PP.CellValueFormatter Text


instance A.FromJSON IndExpenditure where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON IndExpenditure where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}


instance PP.Tabulate IndExpenditure PP.DoNotExpandWhenNested

--makeLenses ''IndExpenditure

indExpenditureFromResultJSON :: A.Value -> Either ByteString IndExpenditure
indExpenditureFromResultJSON val = IndExpenditure
  <$> val |#| "expenditure_date"
  <*> val |#| "expenditure_amount"
  <*> pure (0 :: Amount)
  <*> val |#| "support_oppose_indicator"
  <*> val |#| "office_total_ytd"
  <*> val |#| "category_code_full"
  <*> val |#| "expenditure_description"
  <*> tryKeys ["candidate","candidate_id"] val
  <*> val |#| "committee_id"
  <*> val |#| "transaction_id"
  <*> val |#| "amendment_indicator"
  <*> fmap fromIntegral (sub_idFromText (val |#| "sub_id"))
  <*> pure 0


instance A.FromJSON PartyExpenditure where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON PartyExpenditure where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

--makeLenses ''PartyExpenditure

partyExpenditureFromResultJSON :: CandidateID -> A.Value -> Either ByteString PartyExpenditure
partyExpenditureFromResultJSON cid val = PartyExpenditure
  <$> fmap (utcToLocalTime utc) (val |#| "expenditure_date")
  <*> val |#| "expenditure_amount"
  <*> val |#| "expenditure_purpose_full"
  <*> pure (CandidateKey cid)
  <*> tryKeys ["committee","committee_id"] val -- val |#| "committee_id"
  <*> tryKeys ["committee","name"] val -- val |#| "committee_name"
  <*> fmap fromIntegral (sub_idFromText (val |#| "sub_id"))
  <*> pure 0
