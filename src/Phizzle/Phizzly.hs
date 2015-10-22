{-# LANGUAGE OverloadedStrings #-}

module Phizzle.Phizzly (loadPhishFile, maliciousLink) where

import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Text.Regex.PCRE
import Data.Aeson
import Control.Monad
import Network.URI
import Data.List (isPrefixOf, isSuffixOf)

import Phizzle.Types

-- Reads a phishtank json file as a lazy bytestring.  (Internal function).
getJsonFileData :: String -> IO ByteString
getJsonFileData = LB.readFile

-- Returns either a decoded phishtank json file or an error message.
-- (Internal function).
jsonToWholePhish :: String -> IO (Either String [PhishTank])
jsonToWholePhish jData = eitherDecode <$> getJsonFileData jData

-- Loads a phishtank json file into a parsed list
loadPhishFile :: String -> IO (Either String [PhishTank])
loadPhishFile filepath = jsonToWholePhish $ filepath

-- Maybe returns a successfully parsed uri without the trailing /.
-- (Internal function).
getURIParts :: Maybe URI -> Maybe String
getURIParts uriData = let path    = uriPath <$> uriData
                          regName = uriRegName <$> (join $ uriAuthority <$> uriData)
                      in ( (flip (++)) . dropSlash ) <$> path <*> regName
                      where
    dropSlash :: String -> String
    dropSlash xs
        | "/" `isSuffixOf` xs = init xs
        | otherwise           = xs

-- Maybe returns a bool based on if a link exists in the phishtank file
-- using a regular expression pattern
maliciousLink :: String -> [PhishTank] -> IO (Maybe Bool)
maliciousLink lnk pt = do
        linkExists pt
        where
            linkExists []     = return $ Just False
            linkExists (x:xs) = let lnkParts = getURIParts $ parseURI $ lnk
                                    in case lnkParts of
                                           Nothing -> return Nothing
                                           Just v  -> let choppedLnk = if "www." `isPrefixOf` v
                                                                           then drop 4 v
                                                                           else v
                                                      in case ( (T.unpack (url x)) =~ ("^(http|https)?(://)?(www.)?" ++ choppedLnk ++ "/?$") :: Bool) of
                                                             True  -> return $ Just True
                                                             False -> linkExists xs
