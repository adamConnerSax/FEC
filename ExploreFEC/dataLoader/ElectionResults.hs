{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
module ElectionResults
  (
    loadElectionResults
  )
  where


import qualified OpenFEC.Types          as FEC

import qualified Database.Beam          as B
import qualified Database.Beam.Sqlite   as B
import qualified Database.SQLite.Simple as SL

import qualified Data.FuzzySet          as FS
import           Data.Vinyl             (ElField (..))
import           Data.Vinyl.Curry       (runcurryX)
import           Data.Vinyl.Lens        (rlens, rlens')
import qualified Database.Beam.Sqlite   as B
import           Frames

import qualified Control.Foldl          as FL
import           Control.Lens           (Lens', (.~), (^.))
import           Control.Monad          (join)
import qualified Control.Monad.State    as S
import qualified Data.Foldable          as F
import qualified Data.List              as L
import           Data.List.Split        (chunksOf, splitOn)
import qualified Data.Map               as M
import           Data.Maybe             (isNothing)
import           Data.Monoid            ((<>))
import qualified Data.Sequence          as Seq
import qualified Data.Text              as T
import           Data.Time.Format       (defaultTimeLocale, parseTimeM)
import           Data.Time.LocalTime    (LocalTime)

import GHC.Int (Int32)

electionResultsFile = "./data/resultsHouse2018.csv" -- relative to where you execute
tableTypes "ElectionResults" "../data/resultsHouse2018.csv" -- relative to sub-project ??
{-
lName f = let (Field x) = f ^. rlens @LastName in x

vShare f = let (Field x) = f ^. rlens @VoteShare in x

stateDistrict f =
  let (Field s) = f ^. rlens @State
      (Field d) = f ^. rlens @District
  in (s,d)
-}
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right

loadElectionResults :: SL.Connection
                    -> ([(FEC.Name, FEC.State, FEC.District, FEC.CandidateID)] -> M.Map (FEC.State,FEC.District) (FS.FuzzySet, M.Map Text (FEC.Name, FEC.CandidateID)))
                    -> IO ()
loadElectionResults dbConn candidateMatchMap = do
   -- first get all names and ids from candidate table
  nameStateDistrictId <- B.runBeamSqlite dbConn $ B.runSelectReturningList $ B.select $ do
    candidate <- B.all_ (FEC._openFEC_DB_candidate FEC.openFEC_DB)
    pure (FEC._candidate_name candidate, FEC._candidate_state candidate, FEC._candidate_district candidate, FEC._candidate_id candidate)
  let matcherMap = candidateMatchMap nameStateDistrictId
      addRecord :: (Seq.Seq FEC.ElectionResult, Seq.Seq FEC.ElectionResult)
                -> Record '[State, District, LastName, VoteShare]
                -> (Seq.Seq FEC.ElectionResult, Seq.Seq FEC.ElectionResult)
      addRecord (matched, unmatched) r =
        let sd = (r ^. state, fromIntegral (r ^. district))
            x = do
              (fuzzy, m) <- maybeToEither ("SD Fail: " <> (T.pack $ show sd)) $ M.lookup sd matcherMap
              fuzzyMatch <- maybeToEither ("fuzzyMatchFail: " <> (T.pack $ show sd)) $ FS.getOne fuzzy (T.toUpper $ r ^. lastName)
              maybeToEither ("fuzzyMap fail: " <> (T.pack $ show sd) <> "->" <> fuzzyMatch) $ M.lookup fuzzyMatch m
        in case x of
          Left err             -> (matched, unmatched Seq.|> FEC.ElectionResult (FEC.CandidateKey err) (r ^. lastName) (r ^. voteShare))
          Right (fecName, cid) -> (matched Seq.|> FEC.ElectionResult (FEC.CandidateKey cid) (r ^. lastName) (r ^. voteShare), unmatched)
      addRecordsFold = FL.Fold addRecord (Seq.empty, Seq.empty) id
  loadedRows <- inCoreAoS (readTable electionResultsFile)
  let (matched, unmatched) = FL.fold addRecordsFold loadedRows
  putStrLn $ "Unmatched :"
  mapM_ (putStrLn . show) (F.toList unmatched)
  let matchedL = F.toList matched
  putStrLn $ "inserting " ++ show (L.length matchedL) ++ " election result rows into DB."
  B.runBeamSqlite dbConn $ mapM_ (B.runInsert . B.insert (FEC._openFEC_DB_electionResult FEC.openFEC_DB) . B.insertValues) $ chunksOf 100 $ matchedL
  putStrLn "done inserting election results rows"


