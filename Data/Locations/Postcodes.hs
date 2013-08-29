{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Data.Locations.Postcodes  (getPostcode, getPostcodesInRange, getPostcodeAtLocation, Postcode(..)) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toUpper)
import Network.HTTP

data Postcode = Postcode {_postcode :: String,
                          _latitude :: Float,
                          _longitude :: Float,
                          _constituency :: Maybe String,
                          _district :: Maybe String}
    deriving (Show, Eq)

instance FromJSON Postcode where
    parseJSON (Object v) = Postcode <$> v .: "postcode" 
                                    <*> (fmap read $ getInP "lat" geo)
                                    <*> (fmap read $ getInP "lng" geo)
                                    <*> (fmap Just . getInP "title" . getInP "constituency" $ admin)
                                    <*> (fmap Just . getInP "title" . getInP "district" $ admin)
        where geo          = v .: "geo"
              admin        = v .: "administrative" 
              getInP k p   = flip (.:) k =<< p 

instance FromJSON (Float, Postcode) where
    parseJSON (Object v) = (,) <$> (read <$> v .: "distance")
                               <*> (Postcode <$> v .: "postcode" <*> (read <$> v.: "lat") <*> (read <$> v.: "lng") 
                                             <*> pure Nothing <*> pure Nothing)

baseURL :: String
baseURL = "http://uk-postcodes.com"

-- | Return a Postcode for the postcode string. This uses the network
getPostcode :: String               -- ^ The postcode
            -> IO (Maybe Postcode)  -- ^ Either Just a postcode or Nothing if it could not be found
getPostcode pc = postcodeRequest ("/postcode/" ++ map toUpper pc ++ ".json")

-- | Find all the postcodes within a given range and return a postcode and the distance for each. This uses the network
getPostcodesInRange :: String                         -- ^ The postcode to be the centre of the search
                    -> Float                          -- ^ The radius to search in miles
                    -> IO (Maybe [(Float, Postcode)]) -- ^ Either Just a list of tuples of distance and postcode or Nothing if the given postcode could not be found
getPostcodesInRange pc range = postcodeRequest ("/distance.php?postcode=" ++ pc ++ "&distance=" ++ show range ++ "&format=json")

-- | Find the postcode at the given latitude and longitude
getPostcodeAtLocation :: Float               -- ^ Latitude
                      -> Float               -- ^ Longitude
                      -> IO (Maybe Postcode) -- ^ Either Just the postcode at the give location or Nothing if there is not a postcode there
getPostcodeAtLocation lat lng = postcodeRequest ("/latlng/" ++ show lat ++ "," ++ show lng ++ ".json")

postcodeRequest :: FromJSON a => String -> IO (Maybe a)
postcodeRequest url = do
    response <- getResponseBody =<< simpleHTTP (getRequest $ baseURL ++ url)
    return (decode $ BL.pack response)