{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module OpenFEC.Types where

import           Control.Lens        ((^?))
import           Control.Lens.TH     (makeLenses)
import qualified Data.Aeson          as A
import           Data.Aeson.Lens
import qualified Data.Foldable       as F
import           Data.Scientific     (FPFormat (Fixed), Scientific,
                                      formatScientific)
import           Data.Text           (Text, pack)
import           Data.Time.Calendar  (Day)
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (LocalTime)
import           GHC.Generics        (Generic)
import qualified Text.Tabl           as TT

import qualified OpenFEC.QueryTypes  as QT

-- All fields are prefixed with entity name followed by field name.
-- If the field name begins with entity name (e.g., "candidate_id" in Candidate) we begin with double underscore.
-- So, field name to json key is straightforward

type CandidateID = Text
type Name = Text
type State = Text
type District = Integer

data Candidate = Candidate
  {
    __candidate_id      :: CandidateID
  , _candidate_name     :: Name
  , _candidate_state    :: State
  , _candidate_district :: District
  , _candidate_party    :: QT.Party
  } deriving (Show, Generic)

-- we don't really want to and from JSON of this thing so much as from the candidate query result to this

makeLenses ''Candidate

candidateFromResultJSON :: A.Value -> Maybe Candidate
candidateFromResultJSON val =
  let idM = val ^? key "candidate_id" . _String
      nameM = val ^? key "name" . _String
      stateM = val ^? key "state" . _String
      districtM = val ^? key "district_number" . _Integer
      partyM = val ^? key "party" . _JSON -- this is...magic
  in Candidate <$> idM <*> nameM <*> stateM <*> districtM <*> partyM

candidateHeaders :: [Text]
candidateHeaders = ["ID","Name","State","District","Party"]

candidateAligns = [TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft]

candidateToRow :: Candidate -> [Text]
candidateToRow (Candidate id n s d p) = [id, n, s, pack (show d), pack (show p)]

candidateTable :: (Functor t, Foldable t) => t Candidate -> Text
candidateTable x =
  let cs = F.toList $ candidateToRow <$> x
  in  TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone candidateAligns (candidateHeaders : cs)

type CommitteeID = Text

data Committee = Committee
  {
    __committee_id_       :: CommitteeID
  , _committee_designaton :: Text
  , _committee_name       :: Text
  , __committee_type_full :: Text
  , __committee_type      :: Text -- we should make this its own type, prolly
  } deriving (Generic, Show)

makeLenses ''Committee

committeeFromResultJSON :: A.Value -> Maybe Committee
committeeFromResultJSON val =
  let idM = val ^? key "committee_id" . _String
      dM = val ^? key "designation_full" . _String
      nM = val ^? key "name" . _String
      tfM = val ^? key "committee_type_full" . _String
      tM = val ^? key "committee_type" . _String
  in Committee <$> idM <*> dM <*> nM <*> tfM <*> tM

committeeHeaders :: [Text]
committeeHeaders = ["Committee ID","Designation","Name","Type Full", "Type"]

committeeAligns = [TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft]

committeeToRow :: Committee -> [Text]
committeeToRow (Committee id d n tf t) = [id, d, n, tf, t]

committeeTable :: (Functor t, Foldable t) => t Committee -> Text
committeeTable x =
  let cs = F.toList $ committeeToRow <$> x
  in  TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone committeeAligns (committeeHeaders : cs)

data Filing = Filing
  {
    _filing_cash_on_hand_beginning         :: Scientific
  , _filing_cash_on_hand_end               :: Scientific
  , _filing_committee_type                 :: Text
  , _filing_committee_id                   :: Text
  , _filing_document_type                  :: Text
  , _filing_receipt_date                   :: Day
  , _filing_report_type                    :: Text
  , _filing_total_communication_cost       :: Scientific
  , _filing_total_disbursements            :: Scientific
  , _filing_total_independent_expenditures :: Scientific
  , _filing_total_individual_contributions :: Scientific
  , _filing_total_receipts                 :: Scientific
  , _filing_update_date                    :: Day
  } deriving (Generic, Show)

makeLenses ''Filing

sciAsMoney :: Scientific -> Text
sciAsMoney = pack . formatScientific Fixed (Just 2)

dayToText :: Day -> Text
dayToText = pack . formatTime defaultTimeLocale "%F"

localTimeToText :: LocalTime -> Text
localTimeToText = pack . formatTime defaultTimeLocale "%F"

filingFromResultJSON :: A.Value -> Maybe Filing
filingFromResultJSON val =
  let cohb = val ^? key "cash_on_hand_beginning" . _Number
      cohe = val ^? key "cash_on_hand_end" . _Number
      ct = val ^? key "committee_type" . _String
      ci = val ^? key "committee_id" . _String
      dt = val ^? key "document_type" . _String
      rd = val ^? key "receipt_date" . _JSON
      rt = val ^? key "report_type" . _String
      ttc = val ^? key "total_communication_cost" . _Number
      td = val ^? key "total_disbursements" . _Number
      tie = val ^? key "total_independent_expenditures" . _Number
      tic = val ^? key "total_individual_contributions" . _Number
      tr = val ^? key "total_receipts" . _Number
      ud = val ^? key "update_date" . _JSON
  in Filing <$> cohb <*> cohe <*> ct <*> ci <*> dt <*> rd <*> rt <*> ttc <*> td <*> tie <*> tic <*> tr <*> ud

filingHeaders :: [Text]
filingHeaders = ["$ Beginning", "$ End", "committee type", "committee id", "doc type", "receipt date",
                "report type", "$ communications", "$ disbursements", "$ ind expenditures" ,
                "$ individual contributions", "$ receipts", "update"]


filingAligns = [TT.AlignRight, TT.AlignRight, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft,
                TT.AlignLeft, TT.AlignRight, TT.AlignRight, TT.AlignRight, TT.AlignRight, TT.AlignRight,
                TT.AlignLeft]

filingToRow :: Filing -> [Text]
filingToRow (Filing cohb cohe ct ci dt rd rt ttc td tie tic tr ud) =
  [
    sciAsMoney cohb
  , sciAsMoney cohe
  , ct
  , ci
  , dt
  , dayToText rd
  , rt
  , sciAsMoney ttc
  , sciAsMoney td
  , sciAsMoney tie
  , sciAsMoney tic
  , sciAsMoney tr
  , dayToText ud
  ]

filingTable :: (Functor t, Foldable t) => t Filing -> Text
filingTable x =
  let fs = F.toList $ filingToRow <$> x
  in TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone filingAligns (filingHeaders : fs)


data Report = Report
  {

    _report_total_receipts_period         :: Scientific
  , _report_total_receipts_ytd            :: Scientific
  , _report_total_contributions_period    :: Scientific
  , _report_total_contributions_ytd       :: Scientific
  , _report_total_disbursements_period    :: Scientific
  , _report_total_disbursements_ytd       :: Scientific
  , _report_cash_on_hand_beginning_period :: Scientific
  , _report_receipt_date                  :: LocalTime
  } deriving (Generic, Show)

makeLenses ''Report

reportFromResultJSON :: A.Value -> Maybe Report
reportFromResultJSON val =
  let trpM = val ^? key "total_receipts_period" . _Number
      tryM = val ^? key "total_receipts_ytd" . _Number
      tcpM = val ^? key "total_contributions_period" . _Number
      tcyM = val ^? key "total_contributions_ytd" . _Number
      tdpM = val ^? key "total_disbursements_period" . _Number
      tdyM = val ^? key "total_disbursements_ytd" . _Number
      cohM = val ^? key "cash_on_hand_beginning_period" . _Number
      rdM = val ^? key "receipt_date" . _JSON
  in Report <$> trpM <*> tryM <*> tcpM <*> tcyM <*> tdpM <*> tdyM <*> cohM <*> rdM

reportHeaders :: [Text]
reportHeaders = ["Receipts", "Receipts (YTD)", "Contributions", "Contributions (YTD)", "Disbursements", "Disbursements (YTD)",
                "Cash On Hand", "Receipt Date"]


reportAligns = [TT.AlignRight, TT.AlignRight, TT.AlignRight, TT.AlignRight, TT.AlignRight, TT.AlignRight,
                TT.AlignRight, TT.AlignRight]

reportToRow :: Report -> [Text]
reportToRow (Report trp try tcp tcy tdp tdy coh rd) =
  [
    sciAsMoney trp
  , sciAsMoney try
  , sciAsMoney tcp
  , sciAsMoney tcy
  , sciAsMoney tdp
  , sciAsMoney tdy
  , sciAsMoney coh
  , localTimeToText rd
  ]

reportTable :: (Functor t, Foldable t) => t Report -> Text
reportTable x =
  let fs = F.toList $ reportToRow <$> x
  in TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone reportAligns (reportHeaders : fs)
