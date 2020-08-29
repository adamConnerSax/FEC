{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module OpenFEC.API where


import           Servant
import           Servant.API.Generic
import           Servant.Client
import           Servant.Client.Generic

import           Control.Concurrent     (threadDelay)
import           Control.Exception.Safe (throw)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State    as S
import           Control.RateLimit      (RateLimit (..), rateLimitExecution)
import qualified Data.Aeson             as A
import           Data.ByteString.Char8  (pack)
import qualified Data.Foldable          as F
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Scientific        (Scientific)
import           Data.Text              (Text, unpack)
import qualified Data.Time.Clock        as C
import           Data.Time.LocalTime    (LocalTime)
import           Data.Time.Units        (Microsecond, TimeUnit,
                                         fromMicroseconds)
import           Data.Vector            (Vector)
import qualified Data.Vector            as V

import qualified OpenFEC.Pagination     as FEC
import qualified OpenFEC.Types          as FEC

import GHC.Int (Int32)

baseUrl = BaseUrl Https "api.open.fec.gov" 443 "/v1"

data FEC_Routes route = FEC_Routes
  {
    _candidates :: route
                   :- "candidates"
                   :> QueryParam "api_key" Text
                   :> QueryParams "candidate_status" Text
                   :> QueryParams "office" Text
                   :> QueryParams "party" Text
                   :> QueryParam "state" Text
                   :> QueryParam "district" Int32
                   :> QueryParam "election_year" Int32
                   :> QueryParams "cycle" Int32
                   :> QueryParams "candidate_id" Text
                   :> QueryParam "per_page" Int
                   :> QueryParam "page" Int
                   :> Get '[JSON] FEC.Page
                   
  , _candidates_totals :: route
                          :- "candidates"
                          :> "totals"
                          :> QueryParam "api_key" Text
                          :> QueryParam "is_active_candidate" Bool
                          :> QueryParams "office" Text
                          :> QueryParam "election_year" Int32
                          :> QueryParams "cycle" Int32
                          :> QueryParams "candidate_id" Text
                          :> QueryParam "per_page" Int
                          :> QueryParam "page" Int
                          :> Get '[JSON] FEC.Page
                          
  , _committees :: route
                   :- "committees"
                   :> QueryParam "api_key" Text
                   :> QueryParam "election_year" Int32
                   :> QueryParams "cycle" Int32
                   :> QueryParam "per_page" Int
                   :> QueryParam "page" Int
                   :> Get '[JSON] FEC.Page
                   
  , _committeesByCandidate :: route
                              :- "candidate"
                              :> Capture "candidate_id" Text
                              :> "committees"
                              :> QueryParam "api_key" Text
                              :> QueryParam "election_year" Int32
                              :> QueryParams "cycle" Int32
                              :> QueryParam "per_page" Int
                              :> QueryParam "page" Int
                              :> Get '[JSON] FEC.Page
                              
  , _reports :: route
                :- "committee"
                :> Capture "committee_id" Text
                :> "reports"
                :> QueryParam "api_key" Text
                :> QueryParams "report_type" Text
                :> QueryParams "year" Int32
                :> QueryParams "cycle" Int32
                :> QueryParam "per_page" Int
                :> QueryParam "page" Int
                :> Get '[JSON] FEC.Page
                
  , _disbursements :: route
                      :- "schedules"
                      :> "schedule_b"
                      :> QueryParam "api_key" Text
                      :> QueryParams "committee_id" Text
                      :> QueryParam "two_year_transaction_period" Int32
                      :> QueryParams "recipient_name" Text
                      :> QueryParams "recipient_committee_id" Text
                      :> QueryParam "per_page" Int
                      :> QueryParam "last_index" Int
                      :> QueryParam "last_disbursement_date" LocalTime
                      :> Get '[JSON] A.Value
                      
  , _independent_expenditures :: route
                                 :- "schedules"
                                 :> "schedule_e"
                                 :> QueryParam "api_key" Text
                                 :> QueryParam "candidate_id" Text
                                 :> QueryParam "committee_id" Text
                                 :> QueryParams "cycle" Int32
                                 :> QueryParams "payee_name" Text
                                 :> QueryParam "last_index" Int
                                 :> QueryParam "last_expenditure_date" LocalTime
                                 :> Get '[JSON] A.Value
                                 
  , _party_expenditures :: route
                           :- "schedules"
                           :> "schedule_f"
                           :> QueryParam "api_key" Text
                           :> QueryParam "candidate_id" Text
                           :> QueryParams "cycle" Int32
                           :> QueryParams "payee_name" Text
                           :> QueryParam "per_page" Int
                           :> QueryParam "page" Int
                           :> Get '[JSON] FEC.Page
  }
  deriving (Generic)

fecClients :: FEC_Routes (AsClientT ClientM)
fecClients = genericClient

-- OpenFEC specific constants

type ApiKey = Text
fecApiKey :: ApiKey
fecApiKey = "jjt0sf4z7FVpSAXoTndlnLKl0sDZUFcf3PQjpXrW"

fecMaxPerPage :: Int
fecMaxPerPage = 100

data QueryLimit = QueryLimit { maxQueries :: Int, perTime :: C.DiffTime }
fecQueryLimit = QueryLimit 120 (C.secondsToDiffTime 35)
{-
rateLimited  :: QueryLimit -> (a -> IO b) -> IO (a -> IO b)
rateLimited (QueryLimit n per) action =
  let diffMicros :: Integer = 1 + C.diffTimeToPicoseconds per `div` (fromIntegral $ 1000000 * n) -- 1 + to insure we are slightly slower
      diffTime :: Microsecond = fromMicroseconds diffMicros
  in rateLimitExecution diffTime action
-}

delayQueries :: QueryLimit -> IO ()
delayQueries (QueryLimit n per) =
  let sleepMicros = 1 + C.diffTimeToPicoseconds per `div` (fromIntegral $ 1000000 * n)
  in threadDelay (fromIntegral sleepMicros)

-- specific useful queries

getCandidatesPage :: [FEC.Office] -> [FEC.Party] -> Maybe FEC.State -> Maybe FEC.District -> Maybe FEC.ElectionYear -> [FEC.ElectionYear] -> FEC.PageNumber -> ClientM FEC.Page
getCandidatesPage offices parties stateM districtM electionYearM cycles page =
  (_candidates fecClients) (Just fecApiKey) ["C"] (FEC.officeToText <$> offices) (FEC.partyToText <$> parties) stateM districtM electionYearM cycles [] (Just fecMaxPerPage) (Just page)

getCandidates :: [FEC.Office] -> [FEC.Party] ->  Maybe FEC.State -> Maybe FEC.District -> Maybe FEC.ElectionYear ->  [FEC.ElectionYear] -> ClientM (Vector FEC.Candidate)
getCandidates offices parties stateM districtM electionYearM cycles =
    let getOnePage = getCandidatesPage offices parties stateM districtM electionYearM cycles
    in FEC.getAllPages Nothing FEC.SkipFailed getOnePage FEC.candidateFromResultJSON

getCandidatesByIdPage :: [FEC.CandidateID] -> FEC.PageNumber -> ClientM FEC.Page
getCandidatesByIdPage cids page =
  (_candidates fecClients) (Just fecApiKey) [] [] [] Nothing Nothing Nothing [] cids (Just fecMaxPerPage) (Just page)

getCandidatesById :: [FEC.CandidateID] -> ClientM (Vector FEC.Candidate)
getCandidatesById cids =
  let getOnePage = getCandidatesByIdPage cids
  in FEC.getAllPages Nothing FEC.SkipFailed getOnePage FEC.candidateFromResultJSON

getHouseCandidates :: FEC.State -> FEC.District -> Maybe FEC.ElectionYear -> [FEC.ElectionYear]  -> ClientM (Vector FEC.Candidate)
getHouseCandidates state district electionYearM cycles = getCandidates [FEC.House] [] (Just state) (Just district) electionYearM cycles

getSenateCandidates :: FEC.State -> Maybe FEC.ElectionYear -> [FEC.ElectionYear] -> ClientM (Vector FEC.Candidate)
getSenateCandidates state electionYearM cycles = getCandidates [FEC.Senate] [] (Just state) Nothing electionYearM cycles

getPresidentialCandidates :: FEC.ElectionYear -> ClientM (Vector FEC.Candidate)
getPresidentialCandidates electionYear = getCandidates [FEC.President] [] Nothing Nothing (Just electionYear) []

getCandidatesTotalsPage :: Maybe Bool -> [FEC.Office] -> Maybe FEC.ElectionYear -> [FEC.ElectionYear] -> [FEC.CandidateID] -> FEC.PageNumber -> ClientM FEC.Page
getCandidatesTotalsPage is_activeM offices electionYearM cycles candidates page = 
  (_candidates_totals fecClients) (Just fecApiKey) is_activeM (FEC.officeToText <$> offices) electionYearM cycles candidates (Just fecMaxPerPage) (Just page)

getCandidatesTotals :: Maybe Bool -> [FEC.Office] -> Maybe FEC.ElectionYear -> [FEC.ElectionYear] -> [FEC.CandidateID]
                   -> ClientM (Vector FEC.CandidateTotals)
getCandidatesTotals is_activeM offices electionYearM cycles candidates =
  let getOnePage = getCandidatesTotalsPage is_activeM offices electionYearM cycles candidates
  in FEC.getAllPages Nothing FEC.NoneIfAnyFailed getOnePage FEC.candidateTotalsFromResultJSON

getCommitteesPage :: Maybe FEC.ElectionYear -> [FEC.ElectionYear] -> FEC.PageNumber -> ClientM FEC.Page
getCommitteesPage electionYearM cycles pageN = (_committees fecClients) (Just fecApiKey) electionYearM cycles (Just fecMaxPerPage) (Just pageN)

getCommittees :: Maybe FEC.ElectionYear -> [FEC.ElectionYear] -> ClientM (Vector (FEC.Committee,[FEC.CandidateID]))
getCommittees electionYearM cycles =
  let getOnePage = getCommitteesPage electionYearM cycles
  in FEC.getAllPages Nothing FEC.NoneIfAnyFailed getOnePage FEC.committeeWithCandidatesFromResultJSON

getCommitteesByCandidatePage :: FEC.CandidateID -> Maybe FEC.ElectionYear -> [FEC.ElectionYear] -> FEC.PageNumber -> ClientM FEC.Page
getCommitteesByCandidatePage cid electionYearM cycles page =
  (_committeesByCandidate fecClients) cid (Just fecApiKey) electionYearM cycles (Just fecMaxPerPage) (Just page)

getCommitteesByCandidate :: FEC.CandidateID -> Maybe FEC.ElectionYear -> [FEC.ElectionYear] -> ClientM (Vector FEC.Committee)
getCommitteesByCandidate cid electionYearM cycles =
  let getOnePage = getCommitteesByCandidatePage cid electionYearM cycles
  in FEC.getAllPages Nothing FEC.NoneIfAnyFailed getOnePage FEC.committeeFromResultJSON

getReportsPage :: FEC.CommitteeID -> [Text] -> [FEC.ElectionYear] -> [FEC.ElectionCycle] -> FEC.PageNumber -> ClientM FEC.Page
getReportsPage cid reportTypes reportYears electionCycles page =
  (_reports fecClients) cid (Just fecApiKey) reportTypes reportYears electionCycles (Just fecMaxPerPage) (Just page)

getReports :: FEC.CommitteeID -> [Text] -> [FEC.ElectionYear] -> [FEC.ElectionCycle] -> ClientM (Vector FEC.Report)
getReports cid reportTypes reportYears electionCycles =
  let getOnePage = getReportsPage cid reportTypes reportYears electionCycles
  in FEC.getAllPages Nothing FEC.NoneIfAnyFailed getOnePage  FEC.reportFromResultJSON

{-
getReportsByCandidate :: FEC.CandidateID -> [Text] -> [FEC.ElectionYear] -> [FEC.ElectionCycle] -> ClientM (Maybe (Vector FEC.Report))
getReportsByCandidate id reportType electionYears electionCycles = do
  let getFromCommittee (Committee id _ _ _ _) = getReports id reportTypes reportYears electionCycles
  committeesM <- getCommittees id electionYears
-}

getDisbursementsIPage :: [FEC.CommitteeID] -> FEC.ElectionYear -> [Text] -> [FEC.CommitteeID] -> Maybe Int -> Maybe LocalTime ->  ClientM (FEC.IndexedPage LocalTime)
getDisbursementsIPage cids electionYear recipientNames recipientIds lastIndexM lastDisbursementDateM = do
  json <- (_disbursements fecClients) (Just fecApiKey) cids (Just electionYear) recipientNames recipientIds (Just fecMaxPerPage) lastIndexM lastDisbursementDateM
  let parsedE = FEC.getIndexedPageE "last_disbursement_date" json
  case parsedE of
    Left errBS -> throw $ err417 { errBody = "Decoding Error (Aeson.Value -> FEC.IndexedPage LocalTime) in getDisbursementsIPage. " <> errBS }
    Right ip -> return ip

getDisbursements :: [FEC.CommitteeID] -> FEC.CandidateID -> FEC.ElectionYear -> [Text] -> [FEC.CommitteeID] -> ClientM (Vector FEC.Disbursement)
getDisbursements coids caid electionYear recipientNames recipientIds = do
  let getOnePage x = case x of
        Nothing -> getDisbursementsIPage coids electionYear recipientNames recipientIds Nothing Nothing
        Just (FEC.LastIndex li ldd) -> getDisbursementsIPage coids electionYear recipientNames recipientIds (Just li) (Just ldd)
  raw <- FEC.getAllIndexedPages Nothing FEC.NoneIfAnyFailed getOnePage (FEC.disbursementFromResultJSON caid)
  let nonZero x = FEC._disbursement_amount_adj x /= 0
      adj = V.filter nonZero raw
  liftIO . putStrLn $ "Dropped " ++ show ((V.length raw) - (V.length adj)) ++ " entries from disbursements since they were not for this candidate (" ++ (unpack caid) ++ ")."
  return adj

getIndependentExpendituresByCandidateIPage :: FEC.CandidateID -> [FEC.ElectionYear] -> [Text] -> Maybe Int -> Maybe LocalTime -> ClientM (FEC.IndexedPage LocalTime)
getIndependentExpendituresByCandidateIPage cid cycles payeeNames liM leM = do
  json <- (_independent_expenditures fecClients) (Just fecApiKey) (Just cid) Nothing cycles payeeNames liM leM
  let parsedE = FEC.getIndexedPageE "last_expenditure_date" json
  case parsedE of
    Left errBS -> throw $ err417 { errBody = "Decoding Error (Aeson.Value -> FEC.IndexedPage LocalTime) in getIndependentExpendituresIPageByCandidate. " <> errBS }
    Right ip -> return ip

fixIndEx :: Vector FEC.IndExpenditure -> Vector FEC.IndExpenditure
fixIndEx x =
  -- remove identical on same day
  let f ie = do
        prevByCommitteeOnDay <- S.get
        let cid = FEC._indExpenditure_committee_id ie
            mapKey = (cid, FEC._indExpenditure_date ie)
            prevOnDayL = fromMaybe [] $ M.lookup mapKey prevByCommitteeOnDay
            isDupe = F.elem ie prevOnDayL
        S.put (M.insert mapKey (ie : prevOnDayL) prevByCommitteeOnDay)
        if isDupe then return False else return True
      deduped = S.evalState (V.filterM f x) M.empty
  -- compute new amounts based on ytd since there are still dupes and re-estimates and ...
      g ie = do
        prevByCommittee <- S.get
        let cid = FEC._indExpenditure_committee_id ie
            prevYTDByCommittee = fromMaybe (0 :: FEC.Amount) $ M.lookup cid prevByCommittee
            currYTDByCommittee = FEC._indExpenditure_office_total_ytd ie
            adjAmount = max 0 (currYTDByCommittee - prevYTDByCommittee)
        S.put $ M.insert cid (max currYTDByCommittee prevYTDByCommittee) prevByCommittee
        return $ ie { FEC._indExpenditure_amount_from_ytd = adjAmount }
  in V.reverse $ S.evalState (V.mapM g (V.reverse deduped)) M.empty

getIndependentExpendituresByCommitteeIPage :: FEC.CommitteeID -> [FEC.ElectionYear] -> [Text] -> Maybe Int -> Maybe LocalTime -> ClientM (FEC.IndexedPage LocalTime)
getIndependentExpendituresByCommitteeIPage cid cycles payeeNames liM leM = do
  json <- (_independent_expenditures fecClients) (Just fecApiKey) Nothing (Just cid) cycles payeeNames liM leM
  let parsedE = FEC.getIndexedPageE "last_expenditure_date" json
  case parsedE of
    Left errBS -> throw $ err417 { errBody = "Decoding Error (Aeson.Value -> FEC.IndexedPage LocalTime) in getIndependentExpendituresIPageByCandidate. " <> errBS }
    Right ip -> return ip

-- There are some with no support/oppose indicator.  No idea what to do.  Assume support?  Drop?
getIndependentExpendituresByCandidate :: FEC.CandidateID -> [FEC.ElectionYear] -> [Text] -> ClientM (Vector FEC.IndExpenditure)
getIndependentExpendituresByCandidate cid cycles payeeNames = do
  let getOnePage x = case x of
        Nothing -> getIndependentExpendituresByCandidateIPage cid cycles payeeNames Nothing Nothing
        Just (FEC.LastIndex li led) -> getIndependentExpendituresByCandidateIPage cid cycles payeeNames (Just li) (Just led)
  raw <- FEC.getAllIndexedPages Nothing FEC.SkipFailed getOnePage FEC.indExpenditureFromResultJSON
  let deduped = fixIndEx raw
      num_dupes = V.length raw - V.length deduped
  liftIO . putStrLn $ "Dropped " ++ (show num_dupes) ++ " duplicates from independent expenditures (Candidate ID: " ++ show cid ++ ")."
  return deduped

getIndependentExpendituresByCommittee :: FEC.CommitteeID -> [FEC.ElectionYear] -> [Text] -> ClientM (Vector FEC.IndExpenditure)
getIndependentExpendituresByCommittee cid cycles payeeNames =
  let getOnePage x = case x of
        Nothing -> getIndependentExpendituresByCommitteeIPage cid cycles payeeNames Nothing Nothing
        Just (FEC.LastIndex li led) -> getIndependentExpendituresByCommitteeIPage cid cycles payeeNames (Just li) (Just led)
  in FEC.getAllIndexedPages Nothing FEC.SkipFailed getOnePage FEC.indExpenditureFromResultJSON

getPartyExpendituresPage :: FEC.CandidateID -> [FEC.ElectionYear] -> [Text] -> FEC.PageNumber -> ClientM FEC.Page
getPartyExpendituresPage cid cycles payeeNames page =
  (_party_expenditures fecClients) (Just fecApiKey) (Just cid) cycles payeeNames (Just fecMaxPerPage) (Just page)

getPartyExpenditures :: FEC.CandidateID -> [FEC.ElectionYear] -> [Text] -> ClientM (Vector FEC.PartyExpenditure)
getPartyExpenditures cid cycles payeeNames =
  let getOnePage = getPartyExpendituresPage cid cycles payeeNames
  in FEC.getAllPages Nothing FEC.SkipFailed getOnePage (FEC.partyExpenditureFromResultJSON cid)

