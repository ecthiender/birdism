{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module Birdism.Docs where

import           Servant.Docs
import           System.Directory

import           Birdism.Api
import           Birdism.Lib
import           Birdism.Types


instance ToSample a => ToSample (ApiResponse a)

instance ToSample SearchRequest where
  toSamples _ = samples [ SearchRequest (RegionCode "US-NY-005") (ScientificName "Tytonidae")
                        , SearchRequest (RegionCode "IN-WB-KO") (ScientificName "Cuculidae")
                        , SearchRequest (RegionCode "IN-MN-BI") (ScientificName "Picidae")
                        ]

instance ToSample FamilyScientificNameRequest where
  toSamples _ = singleSample $ FamilyScientificNameRequest (CommonName "Roller")

instance ToSample FamilyScientificNameResponse where
  toSamples _ = singleSample $ FamilyScientificNameResponse relatedFamilies
    where relatedFamilies =
            [ Family (ScientificName "Leptosomidae") (CommonName "Cuckoo-roller")
            , Family (ScientificName "Coraciidae") (CommonName "Rollers")
            , Family (ScientificName "Brachypteraciidae") (CommonName "Ground-Rollers")
            ]

searchResults :: [SearchResultItem]
searchResults =
  [ SearchResultItem (CommonName "Pied Cuckoo") $
      Right [ "https://live.staticflickr.com/xxxx/yyyyyyyyyyyy_xxxxxxxxx.jpg"
            , "https://live.staticflickr.com/xxxxx/yyyyyyyyyyy_xxxxxxxxx.jpg"
            , "https://live.staticflickr.com/xxxxx/yyyyyyyyyyy_xxxxxxxxx.jpg"
            ]
  , SearchResultItem (CommonName "Asian Koel") $
      Right [ "https://live.staticflickr.com/xxxxx/yyyyyyyyyyy_xxxxxxxxxx.jpg"
            , "https://live.staticflickr.com/xxxxx/yyyyyyyyyyy_xxxxxxxxxx.jpg"
            , "https://live.staticflickr.com/xxxxx/yyyyyyyyyyy_xxxxxxxxxx.jpg"
            ]
  ]

instance ToSample SearchResultItem where
  toSamples _ = samples searchResults

instance ToSample Pong where
  toSamples _ = singleSample Pong

instance ToSample Region where
  toSamples _ = samples [ Region (RegionCode "IN-KA") (RegionName "Karnataka")
                        , Region (RegionCode "US-CA-073") (RegionName "San Diego")
                        , Region (RegionCode "IN-WB-KO") (RegionName "Kolkata")
                        ]

families :: [Family]
families = [ Family (ScientificName "Sturnidae") (CommonName "Starlings")
           , Family (ScientificName "Cuculidae") (CommonName "Cuckoos")
           ]

instance ToSample Family where
  toSamples _ = samples families

barnOwl :: Bird
barnOwl = Bird
          (ScientificName "Tyto alba")
          (CommonName "Barn Owl")
          (SpeciesCode "brnowl")
          Nothing
          Nothing
          (CommonName "Barn-Owls")

bluefacedMalkoha :: Bird
bluefacedMalkoha = Bird
                   (ScientificName "Phaenicophaeus viridirostris")
                   (CommonName "Blue-faced Malkoha")
                   (SpeciesCode "blfmal1")
                   Nothing
                   Nothing
                   (CommonName "Cuckoos")

instance ToSample Bird where
  toSamples _ = samples [barnOwl, bluefacedMalkoha]

instance ToSample RedirectLocation where
  toSamples _ = singleSample $ RedirectLocation "/index.html"

instance ToSample CommonName where
  toSamples _ = samples [ CommonName "Barn Owl"
                        , CommonName "Blue-faced Malkoha"
                        ]

apiDocs :: API
apiDocs = docs (pretty serverProxy)

generateDocs :: IO ()
generateDocs = do
  createDirectoryIfMissing False "_docs"
  writeFile "_docs/api_docs.md" $ markdown apiDocs
