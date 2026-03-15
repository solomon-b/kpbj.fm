module Domain.Icecast.StatusSpec (spec) where

--------------------------------------------------------------------------------

import Domain.Icecast.Status (parseListenerCount)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Domain.Icecast.Status" $ do
  describe "parseListenerCount" $ do
    describe "single source (JSON object)" $ do
      it "extracts listener count from a single source" $ do
        parseListenerCount singleSourceJson `shouldBe` Just 5

      it "returns zero when listeners is zero" $ do
        parseListenerCount (mkSingleSource 0) `shouldBe` Just 0

    describe "multiple sources (JSON array)" $ do
      it "sums listeners across all sources" $ do
        parseListenerCount multiSourceJson `shouldBe` Just 8

      it "handles a single-element array" $ do
        parseListenerCount singleElementArrayJson `shouldBe` Just 3

      it "skips non-object entries in the array" $ do
        parseListenerCount arrayWithNonObjectJson `shouldBe` Just 5

    describe "missing or malformed data" $ do
      it "returns Nothing for empty input" $ do
        parseListenerCount "" `shouldBe` Nothing

      it "returns Nothing for invalid JSON" $ do
        parseListenerCount "not json" `shouldBe` Nothing

      it "returns Nothing when icestats key is missing" $ do
        parseListenerCount "{}" `shouldBe` Nothing

      it "returns Nothing when source key is missing" $ do
        parseListenerCount (Aeson.encode $ Aeson.object ["icestats" Aeson..= Aeson.object []])
          `shouldBe` Nothing

      it "returns Nothing for empty source array" $ do
        parseListenerCount emptyArrayJson `shouldBe` Nothing

      it "returns Nothing when listeners key is missing from source" $ do
        parseListenerCount noListenersKeyJson `shouldBe` Nothing

      it "returns Nothing when source is a non-object, non-array value" $ do
        parseListenerCount sourceIsStringJson `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Test fixtures

singleSourceJson :: LBS.ByteString
singleSourceJson = mkSingleSource 5

mkSingleSource :: Int -> LBS.ByteString
mkSingleSource n =
  Aeson.encode $
    Aeson.object
      [ "icestats"
          Aeson..= Aeson.object
            [ "source"
                Aeson..= Aeson.object
                  [ "listeners" Aeson..= n,
                    "server_name" Aeson..= ("KPBJ" :: String)
                  ]
            ]
      ]

multiSourceJson :: LBS.ByteString
multiSourceJson =
  Aeson.encode $
    Aeson.object
      [ "icestats"
          Aeson..= Aeson.object
            [ "source"
                Aeson..= [ Aeson.object ["listeners" Aeson..= (5 :: Int)],
                            Aeson.object ["listeners" Aeson..= (3 :: Int)]
                          ]
            ]
      ]

singleElementArrayJson :: LBS.ByteString
singleElementArrayJson =
  Aeson.encode $
    Aeson.object
      [ "icestats"
          Aeson..= Aeson.object
            [ "source"
                Aeson..= [Aeson.object ["listeners" Aeson..= (3 :: Int)]]
            ]
      ]

arrayWithNonObjectJson :: LBS.ByteString
arrayWithNonObjectJson =
  Aeson.encode $
    Aeson.object
      [ "icestats"
          Aeson..= Aeson.object
            [ "source"
                Aeson..= [ Aeson.object ["listeners" Aeson..= (5 :: Int)],
                            Aeson.String "not a source"
                          ]
            ]
      ]

emptyArrayJson :: LBS.ByteString
emptyArrayJson =
  Aeson.encode $
    Aeson.object
      [ "icestats"
          Aeson..= Aeson.object
            ["source" Aeson..= ([] :: [Aeson.Value])]
      ]

noListenersKeyJson :: LBS.ByteString
noListenersKeyJson =
  Aeson.encode $
    Aeson.object
      [ "icestats"
          Aeson..= Aeson.object
            [ "source"
                Aeson..= Aeson.object
                  ["server_name" Aeson..= ("KPBJ" :: String)]
            ]
      ]

sourceIsStringJson :: LBS.ByteString
sourceIsStringJson =
  Aeson.encode $
    Aeson.object
      [ "icestats"
          Aeson..= Aeson.object
            ["source" Aeson..= ("not an object" :: String)]
      ]
