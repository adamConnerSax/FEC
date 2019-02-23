{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module ExploreFEC.Config where

import           Dhall

data DataSources = DataSources
                   {
                     openFEC_2018_Db               :: FilePath
                   , forecast538_2018House_csv     :: FilePath
                   , electionResults_2018House_csv :: FilePath
                   } deriving (Generic)

instance Interpret DataSources
