{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Phizzle.Types (PhishTank(..), PhishTails(..)) where
    
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data PhishTank  = PhishTank { phish_id :: !Text, url :: !Text, phish_detail_url :: !Text, submission_time :: !Text, verified :: !Text,
                             verification_time :: !Text, online :: !Text, details :: [PhishTails], target :: !Text } deriving (Show, Generic)

data PhishTails = PhishTails { ip_address :: !Text, cidr_block :: !Text, announcing_network :: !Text, rir :: !Text, country :: !Text,
                               detail_time :: !Text } deriving (Show, Generic)

instance FromJSON PhishTank
instance ToJSON PhishTank
instance FromJSON PhishTails
instance ToJSON PhishTails
