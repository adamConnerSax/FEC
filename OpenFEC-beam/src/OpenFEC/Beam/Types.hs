{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module OpenFEC.Beam.Types where


import           Database.Beam             (C)
import qualified Database.Beam             as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Query       as B


import           Control.Monad.Identity    (Identity)
import qualified Data.Aeson                as A
import qualified Data.Aeson.Types          as A
import           Data.Data                 (Data)
import           Data.Scientific           (Scientific)
import           Data.Text                 (Text)
import           Data.Time.Clock           (UTCTime)
import           Data.Time.LocalTime       (LocalTime, utc, utcToLocalTime)
import qualified Data.Vector               as V
import           GHC.Generics              (Generic)


-- query Utilities
runReturningVector :: (B.MonadBeam syntax be handle m, B.FromBackendRow be x) => syntax -> m (V.Vector x)
runReturningVector = fmap V.fromList . B.runReturningList

-- | Run a 'SqlSelect' in a 'MonadBeam' and get the results as a vector
runSelectReturningVector ::
  (B.IsSql92Syntax cmd, B.MonadBeam cmd be hdl m, B.FromBackendRow be a) =>
  B.SqlSelect (B.Sql92SelectSyntax cmd) a -> m (V.Vector a)
runSelectReturningVector (B.SqlSelect s) = runReturningVector (B.selectCmd s)

type CandidateID = Text
type CommitteeID = Text
type Name = Text
type State = Text
type District = Int
type Amount = Double

data Party = Democrat | Republican | Independent | WorkingFamilies | Conservative | Green | Libertarian | Other | Unknown deriving (Show, Read, Enum, Bounded, Eq, Ord, Generic)

data Office = House | Senate | President deriving (Show, Read, Enum, Bounded, Eq, Ord, Generic)

data CandidateT f = Candidate
  {
    _candidate_id       :: C f CandidateID
  , _candidate_name     :: C f Name
  , _candidate_office   :: C f Office
  , _candidate_state    :: C f State
  , _candidate_district :: C f District
  , _candidate_party    :: C f Party
  } deriving (Generic)

Candidate (B.LensFor candidate_id) (B.LensFor candidate_name)
  (B.LensFor candidate_office) (B.LensFor candidate_state)
  (B.LensFor candidate_district) (B.LensFor candidate_party) = B.tableLenses

type Candidate = CandidateT Identity
type CandidateKey = B.PrimaryKey CandidateT Identity

instance A.FromJSON CandidateKey where
  parseJSON = fmap CandidateKey . A.parseJSON

instance A.ToJSON CandidateKey where
  toJSON (CandidateKey x) = A.toJSON x

deriving instance Show Candidate
deriving instance Eq Candidate

instance B.Table CandidateT where
  data PrimaryKey CandidateT f = CandidateKey (C f CandidateID) deriving (Generic)
  primaryKey = CandidateKey . _candidate_id

deriving instance Show CandidateKey
deriving instance Eq CandidateKey

instance B.Beamable CandidateT
instance B.Beamable (B.PrimaryKey CandidateT)

data CommitteeT f = Committee
  {
    _committee_id         :: C f CommitteeID
  , _committee_designaton :: C f (Maybe Text)
  , _committee_name       :: C f (Maybe Text)
  , _committee_type_full  :: C f (Maybe Text)
  , _committee_type       :: C f (Maybe Text) -- we should make this its own type, prolly
  } deriving (Generic)

Committee (B.LensFor committee_id) (B.LensFor committee_designation)
  (B.LensFor committee_name) (B.LensFor committee_type_full)
  (B.LensFor committee_type) = B.tableLenses


type Committee = CommitteeT Identity
type CommitteeKey = B.PrimaryKey CommitteeT Identity

instance A.FromJSON CommitteeKey where
  parseJSON = fmap CommitteeKey . A.parseJSON

instance A.ToJSON CommitteeKey where
  toJSON (CommitteeKey x) = A.toJSON x

deriving instance Show Committee
deriving instance Eq Committee

instance B.Table CommitteeT where
  data PrimaryKey CommitteeT f = CommitteeKey (C f CommitteeID) deriving (Generic)
  primaryKey = CommitteeKey . _committee_id

deriving instance Show CommitteeKey
deriving instance Eq CommitteeKey
deriving instance Ord CommitteeKey

instance B.Beamable CommitteeT
instance B.Beamable (B.PrimaryKey CommitteeT)

data Candidate_x_CommitteeT f = Candidate_x_Committee
  {
    _candidate_x_committee_candidate_id :: B.PrimaryKey CandidateT f
  , _candidate_x_committee_committee_id :: B.PrimaryKey CommitteeT f
  } deriving (Generic)

type Candidate_x_Committee = Candidate_x_CommitteeT Identity

deriving instance Show Candidate_x_Committee
deriving instance Eq Candidate_x_Committee

instance B.Table Candidate_x_CommitteeT where
  data PrimaryKey Candidate_x_CommitteeT f = Candidate_x_CommitteeKey (B.PrimaryKey CandidateT f) (B.PrimaryKey CommitteeT f) deriving (Generic)
  primaryKey x = Candidate_x_CommitteeKey (_candidate_x_committee_candidate_id x) (_candidate_x_committee_committee_id x)

type Candidate_x_CommitteeKey = B.PrimaryKey Candidate_x_CommitteeT Identity

instance B.Beamable Candidate_x_CommitteeT
instance B.Beamable (B.PrimaryKey Candidate_x_CommitteeT)

committeesCandidateRelationship :: B.ManyToMany OpenFEC_DB CandidateT CommitteeT
committeesCandidateRelationship = B.manyToMany_ (_openFEC_DB_candidate_x_committee openFEC_DB) _candidate_x_committee_candidate_id _candidate_x_committee_committee_id

data DisbursementT f = Disbursement
  {
    _disbursement_date              :: C f LocalTime
  , _disbursement_amount            :: C f Amount
  , _disbursement_amount_adj        :: C f Amount
  , _disbursement_num_candidates    :: C f Int
  , _disbursement_purpose_Category  :: C f Text
  , _disbursement_recipient_name    :: C f (Maybe Text)
  , _disbursement_candidate_id      :: B.PrimaryKey CandidateT f
  , _disbursement_committee_id      :: B.PrimaryKey CommitteeT f
  , _disbursement_line_number_label :: C f (Maybe Text)
  , _disbursement_recipient_zip     :: C f (Maybe Text)
  , _disbursement_sub_id            :: C f Int
  , _disbursement_id                :: C f Int
  } deriving (Generic)

Disbursement (B.LensFor disbursement_date) (B.LensFor disbursement_amount)
  (B.LensFor disbursement_amount_adj) (B.LensFor disbursement_num_candidates)
  (B.LensFor disbursement_purpose_category) (B.LensFor disbursement_recipient_name)
  (CandidateKey (B.LensFor disbursement_candidate_id))
  (CommitteeKey (B.LensFor disbursement_committee_id)) (B.LensFor disbursement_line_number_label)
  (B.LensFor disbursement_recipient_zip)
  (B.LensFor disbursement_sub_id) (B.LensFor disbursement_id) = B.tableLenses

type Disbursement = DisbursementT Identity
type DisbursementKey = B.PrimaryKey DisbursementT Identity

deriving instance Show Disbursement
deriving instance Eq Disbursement

instance B.Table DisbursementT where
  data PrimaryKey DisbursementT f = DisbursementKey (C f Int) deriving (Generic)
  primaryKey = DisbursementKey . _disbursement_id

instance B.Beamable DisbursementT
instance B.Beamable (B.PrimaryKey DisbursementT)

data SpendingIntention = Support | Oppose deriving (Generic, Show, Read, Enum, Ord, Bounded, Eq, Data)

data IndExpenditureT f  = IndExpenditure
  {
    _indExpenditure_date                     :: C f LocalTime
  , _indExpenditure_amount                   :: C f Amount
  , _indExpenditure_amount_from_ytd          :: C f Amount
  , _indExpenditure_support_oppose_indicator :: C f SpendingIntention
  , _indExpenditure_office_total_ytd         :: C f Amount
  , _indExpenditure_category_code_full       :: C f (Maybe Text)
  , _indExpenditure_description              :: C f (Maybe Text)
  , _indExpenditure_candidate_id             :: B.PrimaryKey CandidateT f
  , _indExpenditure_committee_id             :: B.PrimaryKey CommitteeT f
  , _indExpenditure_payee_name               :: C f (Maybe Text)
  , _indExpenditure_payee_zip                :: C f (Maybe Text)
  , _indExpenditure_sub_id                   :: C f Int
  , _indExpenditure_id                       :: C f Int
  } deriving (Generic)

IndExpenditure (B.LensFor indExpenditure_date) (B.LensFor indExpenditure_amount)
  (B.LensFor indExpenditure_amount_from_ytd) (B.LensFor indExpenditure_support_oppose_indicator)
  (B.LensFor indExpenditure_office_total_ytd) (B.LensFor indExpenditure_category_code_full)
  (B.LensFor indExpenditure_description) (CandidateKey (B.LensFor indExpenditure_candidate_id))
  (CommitteeKey (B.LensFor indExpenditure_committee_id))
  (B.LensFor indExpenditure_payee_name) (B.LensFor indExpenditure_payee_zip)
  (B.LensFor indExpenditure_sub_id) (B.LensFor indExpenditure_id) = B.tableLenses

type IndExpenditure = IndExpenditureT Identity
type IndExpenditureKey = B.PrimaryKey IndExpenditureT Identity

deriving instance Show IndExpenditure
deriving instance Eq IndExpenditure

instance B.Table IndExpenditureT where
  data PrimaryKey IndExpenditureT f = IndExpenditureKey (C f Int) deriving (Generic)
  primaryKey = IndExpenditureKey . _indExpenditure_id

instance B.Beamable IndExpenditureT
instance B.Beamable (B.PrimaryKey IndExpenditureT)

data PartyExpenditureT f = PartyExpenditure
  {
    _partyExpenditure_date           :: C f LocalTime
  , _partyExpenditure_amount         :: C f Amount
  , _partyExpenditure_purpose_full   :: C f (Maybe Text)
  , _partyExpenditure_candidate_id   :: B.PrimaryKey CandidateT f
  , _partyExpenditure_committee_id   :: B.PrimaryKey CommitteeT f
  , _partyExpenditure_committee_name :: C f Text
  , _partyExpenditure_sub_id         :: C f Int
  , _partyExpenditure_id             :: C f Int
  } deriving (Generic)

PartyExpenditure (B.LensFor partyExpenditure_date) (B.LensFor partyExpenditure_amount)
  (B.LensFor partyExpenditure_purpose_full) (CandidateKey (B.LensFor partyExpenditure_candidate_id))
  (CommitteeKey (B.LensFor partyExpenditure_committee_id))
  (B.LensFor partyExpenditure_committee_name)
  (B.LensFor partyExpenditure_sub_id) (B.LensFor partyExpenditure_id) = B.tableLenses

type PartyExpenditure = PartyExpenditureT Identity
type PartyExpenditureKey = B.PrimaryKey PartyExpenditureT Identity

deriving instance Show PartyExpenditure
deriving instance Eq PartyExpenditure

instance B.Table PartyExpenditureT where
  data PrimaryKey PartyExpenditureT f = PartyExpenditureKey (C f Int) deriving (Generic)
  primaryKey = PartyExpenditureKey . _partyExpenditure_id

instance B.Beamable PartyExpenditureT
instance B.Beamable (B.PrimaryKey PartyExpenditureT)

data CandidateIdOnlyT f = CandidateIdOnly { _candidate_id_only :: B.PrimaryKey CandidateT f } deriving (Generic)

CandidateIdOnly (CandidateKey (B.LensFor candidate_id_only)) = B.tableLenses

type CandidateIdOnly = CandidateIdOnlyT Identity

deriving instance Show CandidateIdOnly

instance B.Table CandidateIdOnlyT where
  data PrimaryKey CandidateIdOnlyT f = CandidateIdOnlyKey (B.PrimaryKey CandidateT f) deriving (Generic)
  primaryKey = CandidateIdOnlyKey . _candidate_id_only

instance B.Beamable CandidateIdOnlyT
instance B.Beamable (B.PrimaryKey CandidateIdOnlyT)

data Forecast538T f = Forecast538
  {
    _forecast538_forecast_date  :: C f LocalTime
  , _forecast538_candidate_id   :: B.PrimaryKey CandidateT f
  , _forecast538_candidate_name :: C f Text
  , _forecast538_incumbent      :: C f Bool
  , _forecast538_model          :: C f Text
  , _forecast538_winP           :: C f Double
  , _forecast538_voteshare      :: C f Double
  , _forecast538_voteshare10    :: C f Double
  , _forecast538_voteshare90    :: C f Double
  , _forecast538_id             :: C f Int
  } deriving (Generic)

Forecast538 (B.LensFor forecast538_forecast_date) (CandidateKey (B.LensFor forecast538_candidate_id))
  (B.LensFor forecast538_candidate_name) (B.LensFor forecast538_incumbent)
  (B.LensFor forecast538_model) (B.LensFor forecast538_winP)
  (B.LensFor forecast538_voteshare)   (B.LensFor forecast538_voteshare10)
  (B.LensFor forecast538_voteshare90) (B.LensFor forecast538_id) = B.tableLenses

type Forecast538 = Forecast538T Identity
type Forecast538Key = B.PrimaryKey Forecast538T Identity

deriving instance Show Forecast538
deriving instance Eq Forecast538

instance B.Table Forecast538T where
  data PrimaryKey Forecast538T f = Forecast538Key (C f Int) deriving (Generic)
  primaryKey = Forecast538Key . _forecast538_id

instance B.Beamable Forecast538T
instance B.Beamable (B.PrimaryKey Forecast538T)

data OpenFEC_DB f = OpenFEC_DB
  {
    _openFEC_DB_candidate :: f (B.TableEntity CandidateT)
  , _openFEC_DB_committee :: f (B.TableEntity CommitteeT)
  , _openFEC_DB_candidate_x_committee :: f (B.TableEntity Candidate_x_CommitteeT)
  , _openFEC_DB_disbursement :: f (B.TableEntity DisbursementT)
  , _openFEC_DB_indExpenditure :: f (B.TableEntity IndExpenditureT)
  , _openFEC_DB_partyExpenditure :: f (B.TableEntity PartyExpenditureT)
  , _openFEC_DB_forecast538 :: f (B.TableEntity Forecast538T)
  , _openFEC_DB_candidate_to_load :: f (B.TableEntity CandidateIdOnlyT)
  } deriving (Generic)

OpenFEC_DB (B.TableLens openFEC_DB_candidate) (B.TableLens openFEC_DB_committee)
  (B.TableLens openFEC_DB_candidate_x_committee) (B.TableLens openFEC_DB_disbursement)
  (B.TableLens openFEC_DB_indExpenditure) (B.TableLens openFEC_DB_partyExpenditure)
  (B.TableLens openFEC_forecast538) (B.TableLens openFEC_DB_candidate_to_load) = B.dbLenses

instance B.Database be OpenFEC_DB

openFEC_DB :: B.DatabaseSettings be OpenFEC_DB
openFEC_DB = B.defaultDbSettings


