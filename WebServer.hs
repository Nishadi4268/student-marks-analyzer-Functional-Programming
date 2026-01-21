{-# LANGUAGE OverloadedStrings #-}

-- Minimal web server to expose the analyzer results as JSON and a simple UI
-- Uses `scotty` to provide endpoints. Install with `cabal install scotty aeson`.

module Main where

import Web.Scotty
import DataTypes
import IOHandler (readStudentsFromFile, writeSummaryToFile)
import Processing (studentAverage, studentSummary, cohortAverage, gradeDistribution)
import Data.Aeson (object, (.=), Value, FromJSON(..), (.:), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class (liftIO)
import System.IO (appendFile)
import qualified Control.Exception as E

-- JSON request format for adding a student
data AddStudentRequest = AddStudentRequest
  { reqSid :: String
  , reqName :: String
  , reqMarks :: [Int]
  } deriving (Show)

instance FromJSON AddStudentRequest where
  parseJSON = withObject "AddStudentRequest" $ \v ->
    AddStudentRequest
      <$> v .: "sid"
      <*> v .: "name"
      <*> v .: "marks"

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

-- Write a student to CSV file
appendStudentToFile :: FilePath -> Student -> IO ()
appendStudentToFile path st = do
  let marksStr = unwords $ map (\m -> show m ++ ";") (init (marks st)) ++ [show (last (marks st))]
      marksStr' = take (length marksStr - 1) marksStr  -- Remove trailing semicolon from last iteration, correct below
      marksStr'' = concatMap (\m -> show m ++ ";") (init (marks st)) ++ show (last (marks st))
      csvLine = sid st ++ "," ++ name st ++ "," ++ marksStr'' ++ "\n"
  appendFile path csvLine

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

  -- API: add new student
  post "/api/add-student" $ do
    req <- jsonData :: ActionM AddStudentRequest
    
    -- Validate input
    if null (reqSid req) || null (reqName req) || null (reqMarks req)
      then text "Error: Missing required fields (sid, name, marks)"
      else if any (\m -> m < 0 || m > 100) (reqMarks req)
        then text "Error: All marks must be between 0 and 100"
        else do
          -- Create student record
          let newStudent = Student (reqSid req) (reqName req) (reqMarks req)
          
          -- Append to CSV file
          result <- liftIO $ E.catch
            (appendStudentToFile "students.csv" newStudent >> return True)
            (\(e :: E.SomeException) -> do
              putStrLn $ "Error writing to file: " ++ show e
              return False
            )
          
          if result
            then json $ object ["message" .= ("Student added successfully" :: String), "student" .= studentToJson newStudent]
            else text "Error: Failed to write student to file"

  -- Simple health endpoint
  get "/health" $ text "OK"

