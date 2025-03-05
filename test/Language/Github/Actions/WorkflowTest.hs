{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Github.Actions.WorkflowTest where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as YAML
import Hedgehog (Gen, Property, forAll, property, tripping)
import Language.Github.Actions.Workflow (Workflow)
import qualified Language.Github.Actions.Workflow as Workflow
import System.FilePath (takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.Golden.Extra.GoldenVsToYAML (goldenVsToYaml)
import Text.Show.Pretty (ppShow)

test_goldenWorkflowFromYaml :: IO TestTree
test_goldenWorkflowFromYaml = do
  testYamlFiles <-
    filter (not . (reverse ".golden.yml" `isPrefixOf`) . reverse)
      <$> findByExtension [".yml"] "test/golden"
  pure $
    testGroup
      "Yaml Roundtrip"
      [ runGoldenVsToYamlFileTest testYamlFilePath
        | testYamlFilePath <- testYamlFiles
      ]
  where
    runGoldenVsToYamlFileTest testYamlFilePath =
      let outputFilePath =
            (<> ".golden.yml")
              . reverse
              . drop 4
              $ reverse testYamlFilePath
          haskellOutputFilePath =
            (<> ".hs.txt")
              . reverse
              . drop 4
              $ reverse testYamlFilePath
       in goldenVsToYaml
            ("roundtrip " <> takeBaseName testYamlFilePath)
            testYamlFilePath
            $ do
              putStrLn $ "roundtrip " <> takeBaseName testYamlFilePath
              eitherWorkflow <- YAML.decodeFileEither @Workflow testYamlFilePath
              either
                (BS.writeFile outputFilePath)
                (writeOutputFiles outputFilePath haskellOutputFilePath)
                $ first (encodeUtf8 . Text.pack . YAML.prettyPrintParseException) eitherWorkflow
    writeOutputFiles :: FilePath -> FilePath -> Workflow -> IO ()
    writeOutputFiles outputFilePath haskellOutputFilePath workflow = do
      BS.writeFile outputFilePath (YAML.encode workflow)
        >> BS.writeFile haskellOutputFilePath (encodeUtf8 . Text.pack $ ppShow workflow)

hprop_WorkflowRoundTrip :: Property
hprop_WorkflowRoundTrip =
  trippingJSON Workflow.gen

trippingJSON :: (Eq a, Show a, Aeson.FromJSON a, Aeson.ToJSON a) => Gen a -> Property
trippingJSON gen = property $ do
  a <- forAll gen
  tripping a Aeson.encode Aeson.eitherDecode
