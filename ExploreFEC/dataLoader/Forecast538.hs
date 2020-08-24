{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
module Forecast538
  (
    load538ForecastData
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
import qualified Data.Sequence          as Seq
import qualified Data.Text              as T
import           Data.Time.Format       (defaultTimeLocale, parseTimeM)
import           Data.Time.LocalTime    (LocalTime)

import GHC.Int (Int32)

houseForecastFile = "../data/house_district_forecast.csv"
tableTypes "HouseForecast538" "../data/house_district_forecast.csv"

starting538NameMap = M.fromList
  [
    ("Others", Just ("OTHERS","N/A"))
  , ("Phillip Aronoff",Just ("PHILLIP ARNOLD ARONOFF","H8TX29094"))
  , ("Rick Tyler", Just ("TYLER, RICK", "H6TN04218"))
  ]

decodeFrame :: Int32
  -> Record '[Forecastdate, State, District, Special, Candidate, Party, Incumbent, Model, WinProbability, Voteshare, P10Voteshare, P90Voteshare]
  -> Maybe FEC.Forecast538
decodeFrame id f =
  let (Field dateT) = f ^. rlens @Forecastdate
      dateM = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (T.unpack dateT) :: Maybe LocalTime
      (Field state) = f ^. rlens @State
      (Field district) = f ^. rlens @District
      (Field special) = f ^. rlens @Special
      (Field name) = f ^. rlens @Candidate
      (Field party) = f ^. rlens @Party
      (Field incumbent) = f ^. rlens @Incumbent
      (Field model) = f ^. rlens @Model
      (Field winP) = f ^. rlens @WinProbability
      (Field voteshare) = f ^. rlens @Voteshare
      (Field p10_vs) = f ^. rlens @P10Voteshare
      (Field p90_vs) = f ^. rlens @P90Voteshare
  in FEC.Forecast538 <$> dateM <*> pure (FEC.CandidateKey "") <*> pure name <*> pure incumbent <*> pure model <*> pure winP <*> pure voteshare <*> pure p10_vs <*> pure p90_vs <*> pure id


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right

getIdAndUpdateMap :: Text
                  -> M.Map Text (Maybe (FEC.Name, FEC.CandidateID))
                  -> FEC.State
                  -> FEC.District
                  -> M.Map (FEC.State, FEC.District) (FS.FuzzySet, M.Map Text (FEC.Name, FEC.CandidateID))
                  -> (Maybe FEC.CandidateID, M.Map Text (Maybe (FEC.Name, FEC.CandidateID)))
getIdAndUpdateMap name idByName state district fuzzyAndMapByStateDistrict =
  case (fmap snd . join $ M.lookup name idByName) of
    Just cid -> (Just cid, idByName)
    Nothing ->
      let x = do
            (fuzzy, m) <- maybeToEither "SD Fail" $ M.lookup (state, district) fuzzyAndMapByStateDistrict
            fuzzyMatch <- maybeToEither "fuzzyMatch fail" $  FS.getOne fuzzy (toUpperLastName name)
            maybeToEither "fuzzyMap fail" $ M.lookup fuzzyMatch m
      in case x of
        Left err -> (Just err, M.insert name Nothing idByName)
        Right (fecName, cid) -> (Just cid, M.insert name (Just (fecName, cid)) idByName)

toUpperLastName fullName =
  let notNameL = ["JR.","II","III","IV"]
      (x, end) = T.breakOnEnd " " fullName
      end' = T.toUpper end
      in if T.strip end' `L.elem` notNameL then T.toUpper ( snd  $ T.breakOnEnd " " $ T.strip x) else end'

load538ForecastData :: SL.Connection
                    -> ([(FEC.Name, FEC.State, FEC.District, FEC.CandidateID)] -> M.Map (FEC.State,FEC.District) (FS.FuzzySet, M.Map Text (FEC.Name, FEC.CandidateID)))
                    -> IO ()
load538ForecastData dbConn candidateNameMatchMap = do
  -- first get all names and ids from candidate table
  nameStateDistrictId <- B.runBeamSqlite dbConn $ B.runSelectReturningList $ B.select $ do
    candidate <- B.all_ (FEC._openFEC_DB_candidate FEC.openFEC_DB)
    pure (FEC._candidate_name candidate, FEC._candidate_state candidate, FEC._candidate_district candidate, FEC._candidate_id candidate)
  let matcherMap = candidateNameMatchMap nameStateDistrictId
      getFECId :: Seq.Seq FEC.Forecast538
               -> Record '[Forecastdate, State, District, Special, Candidate, Party, Incumbent, Model, WinProbability, Voteshare, P10Voteshare, P90Voteshare]
               -> S.State (Int32, M.Map Text (Maybe (FEC.Name, FEC.CandidateID))) (Seq.Seq FEC.Forecast538)
      getFECId fs x = do
        (nextId, matched) <- S.get
        case decodeFrame nextId x of
          Nothing -> return fs -- frame not decoded properly
          Just fcast -> do
            let name538 = (FEC._forecast538_candidate_name fcast)
                (Field state) = x ^. rlens @State
                (Field district) = fromIntegral (x ^. rlens @District)
                (idM, newMatched) = getIdAndUpdateMap name538 matched state district matcherMap
                cid = maybe ("N/A") id idM
            S.put (nextId + 1, newMatched)
            return (fs Seq.|> fcast {FEC._forecast538_candidate_id = FEC.CandidateKey cid})
      getFECIdFoldM = FL.FoldM getFECId (return Seq.empty) return
  -- trying to do in memory first.  May need to stream it...
  loadedRows <- inCoreAoS (readTable houseForecastFile)
  -- first pass, get unique names and match them
  let (forecast538DbRows,(_,names538Map)) = S.runState (FL.foldM getFECIdFoldM loadedRows) (0,starting538NameMap)
  putStrLn $ "unmatched names: "
  print $ M.filter isNothing names538Map
  putStrLn $ "Inserting " ++ show (Seq.length forecast538DbRows) ++ " 538 forecast rows into DB."
  B.runBeamSqlite dbConn $ mapM_ (B.runInsert . B.insert (FEC._openFEC_DB_forecast538 FEC.openFEC_DB) . B.insertValues) $ chunksOf 90 $ F.toList $ forecast538DbRows
  putStrLn $ "done inserting forecast rows."
  return ()
