{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Github.Actions.WorkflowTest where

import Data.Yaml qualified as YAML
import Hedgehog.Extended (Property, trippingJSON)
import Language.Github.Actions.Workflow (Workflow)
import Language.Github.Actions.Workflow qualified as Workflow
import Relude
import System.FilePath (takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.Golden.Yaml.Diff (goldenVsToYamlFile)
import Text.Show.Pretty (ppShow)

test_goldenWorkflowFromYaml :: IO TestTree
test_goldenWorkflowFromYaml = do
  goldenYamlFiles <-
    filter ((reverse ".golden.yml" `isPrefixOf`) . reverse)
      <$> findByExtension [".yml"] "test/golden"
  pure $
    testGroup
      "Yaml Roundtrip"
      [ runGoldenVsToYamlFileTest goldenYamlFilePath
        | goldenYamlFilePath <- goldenYamlFiles
      ]
  where
    runGoldenVsToYamlFileTest goldenYamlFilePath =
      let outputFilePath =
            (<> ".yml") . reverse . drop 11 $
              reverse goldenYamlFilePath
          haskellOutputFilePath =
            (<> ".hs.txt") . reverse . drop 11 $
              reverse goldenYamlFilePath
       in goldenVsToYamlFile
            ("roundtrip " <> takeBaseName goldenYamlFilePath)
            goldenYamlFilePath
            outputFilePath
            $ do
              eitherWorkflow <- YAML.decodeFileEither @Workflow goldenYamlFilePath
              either
                (writeFileBS outputFilePath)
                (writeOutputFiles outputFilePath haskellOutputFilePath)
                $ first (encodeUtf8 . YAML.prettyPrintParseException) eitherWorkflow
    writeOutputFiles :: FilePath -> FilePath -> Workflow -> IO ()
    writeOutputFiles outputFilePath haskellOutputFilePath workflow = do
      writeFileBS outputFilePath (YAML.encode workflow)
        >> writeFileBS haskellOutputFilePath (encodeUtf8 $ ppShow workflow)

hprop_WorkflowRoundTrip :: Property
hprop_WorkflowRoundTrip =
  trippingJSON Workflow.gen
