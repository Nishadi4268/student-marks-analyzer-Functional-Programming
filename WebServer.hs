{-# LANGUAGE OverloadedStrings #-}

-- Minimal web server to expose the analyzer results as JSON and a simple UI
-- Uses `scotty` to provide endpoints. Install with `cabal install scotty aeson`.

module Main where

import Web.Scotty
import DataTypes
import IOHandler (readStudentsFromFile)
import Processing (studentAverage, studentSummary, cohortAverage, gradeDistribution)
import Data.Aeson (object, (.=), Value)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class (liftIO)

-- Convert Grade to String
gradeToStr :: Grade -> String
gradeToStr A = "A"
gradeToStr B = "B"
gradeToStr C = "C"
gradeToStr D = "D"
gradeToStr F = "F"

-- Convert a Student and its computed summary to a JSON Value
studentToJson :: Student -> Value
studentToJson s =
  let Summary avg mx mn g = studentSummary s
  in object [ "sid" .= sid s
            , "name" .= name s
            , "marks" .= marks s
            , "average" .= avg
            , "max" .= mx
            , "min" .= mn
            , "grade" .= gradeToStr g
            ]

summaryToJson :: [Student] -> Value
summaryToJson sts =
  object [ "count" .= length sts
         , "cohortAverage" .= cohortAverage sts
         , "gradeDistribution" .= map toObj (gradeDistribution sts)
         ]
  where
    toObj (g, c) = object [ "grade" .= gradeToStr g, "count" .= c ]

main :: IO ()
main = scotty 3000 $ do
  -- Serve static index
  get "/" $ file "static/index.html"
  get "/static/:file" $ do
    f <- param "file"
    file $ "static/" ++ (TL.unpack f)

  -- API: list students (reads students.csv in project root)
  get "/api/students" $ do
    sts <- liftIO $ readStudentsFromFile "students.csv"
    json $ Aeson.toJSON $ map studentToJson sts

  -- API: report summary
  get "/api/report" $ do
    sts <- liftIO $ readStudentsFromFile "students.csv"
    json $ summaryToJson sts

  -- Simple health endpoint
  get "/health" $ text "OK"
