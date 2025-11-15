module IOHandler
  ( readStudentsFromFile
  , readStudentsFromConsole
  , writeSummaryToFile
  , printReport
  ) where

import DataTypes
import Processing
import Utils (formatDouble, trim)
import System.IO (withFile, IOMode(..), hPutStrLn)
import Control.Monad (forM_)

-- Read file, parse lines into Students, ignoring invalid lines but reporting count
readStudentsFromFile :: FilePath -> IO [Student]
readStudentsFromFile path = do
  content <- readFile path
  let ls = filter (not . null) $ lines content
      parsed = map (\l -> (l, parseStudentLine l)) ls
      good = [s | (_, Just s) <- parsed]
      badCount = length [() | (_, Nothing) <- parsed]
  putStrLn $ "Loaded " ++ show (length good) ++ " students."
  if badCount > 0
    then putStrLn $ "Ignored " ++ show badCount ++ " invalid lines."
    else return ()
  return good

-- Simple console-based entry: user enters lines in the same CSV format; empty line ends
readStudentsFromConsole :: IO [Student]
readStudentsFromConsole = do
  putStrLn "Enter student lines in format: StudentID,Name,mark1;mark2;... "
  putStrLn "Enter empty line to finish."
  loop []
  where
    loop acc = do
      putStr "> "
      line <- getLine
      if null (trim line)
        then return (reverse acc)
        else case parseStudentLine line of
               Just s -> loop (s:acc)
               Nothing -> do
                 putStrLn "Invalid line; try again."
                 loop acc
    -- using `trim` from Utils to remove surrounding whitespace

-- Write a short report to file
writeSummaryToFile :: FilePath -> [Student] -> IO ()
writeSummaryToFile path sts = withFile path WriteMode $ \h -> do
  hPutStrLn h "Student Marks Analyzer Report"
  hPutStrLn h $ "Number of students: " ++ show (length sts)
  hPutStrLn h $ "Cohort average: " ++ formatDouble (cohortAverage sts)
  hPutStrLn h ""
  hPutStrLn h "Per-student summaries:"
  forM_ sts $ \s ->
    let Summary avg mx mn g = studentSummary s
    in hPutStrLn h $ sid s ++ "," ++ name s ++ "," ++ formatDouble avg ++ "," ++ show mx ++ "," ++ show mn ++ "," ++ show g

-- Pretty print report to console
printReport :: [Student] -> IO ()
printReport sts = do
  putStrLn "====== Student Marks Analyzer Report ======"
  putStrLn $ "Number of students: " ++ show (length sts)
  putStrLn $ "Cohort average: " ++ formatDouble (cohortAverage sts)
  putStrLn ""
  putStrLn "Top 5 students:"
  forM_ (topStudents 5 sts) $ \(s, avg) ->
    putStrLn $ " - " ++ name s ++ " (" ++ sid s ++ "): " ++ formatDouble avg
  putStrLn ""
  putStrLn "Grade distribution:"
  forM_ (gradeDistribution sts) $ \(g, c) ->
    putStrLn $ " - " ++ show g ++ ": " ++ show c
  putStrLn ""
  putStrLn "Per-student summaries:"
  forM_ sts $ \s ->
    let Summary avg mx mn g = studentSummary s
    in putStrLn $ name s ++ " (" ++ sid s ++ ") -> avg: " ++ formatDouble avg ++ ", max: " ++ show mx ++ ", min: " ++ show mn ++ ", grade: " ++ show g
  putStrLn "==========================================="
