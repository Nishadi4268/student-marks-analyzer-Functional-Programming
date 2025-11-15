module Main where

import IOHandler
import DataTypes
import Processing
import Utils (formatDouble)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "=== Student Marks Analyzer ==="
  mainMenu []

mainMenu :: [Student] -> IO ()
mainMenu st = do
  putStrLn ""
  putStrLn "Menu:"
  putStrLn "1) Load students from CSV file"
  putStrLn "2) Enter students manually (console)"
  putStrLn "3) Show cohort report"
  putStrLn "4) Show top N students"
  putStrLn "5) Export report to file"
  putStrLn "0) Exit"
  putStr "Select an option: "
  hFlush stdout
  opt <- getLine
  case opt of
    "1" -> do
      putStr "Enter CSV path: "
      hFlush stdout
      p <- getLine
      students <- readStudentsFromFile p
      mainMenu students
    "2" -> do
      students <- readStudentsFromConsole
      mainMenu students
    "3" -> do
      if null st
        then putStrLn "No students loaded. Load students first."
        else printReport st
      mainMenu st
    "4" -> do
      if null st
        then putStrLn "No students loaded."
        else do
          putStr "Enter N: "
          hFlush stdout
          nStr <- getLine
          case reads nStr :: [(Int, String)] of
            [(n, "")] -> do
              let tops = topStudents n st
              putStrLn $ "Top " ++ show n ++ ":"
              mapM_ (\(ix, (s, avg)) -> putStrLn $ show ix ++ ") " ++ name s ++ " - " ++ formatDouble avg) (zip [1..] tops)
            _ -> putStrLn "Invalid number."
      mainMenu st
    "5" -> do
      if null st
        then putStrLn "No students loaded."
        else do
          putStr "Enter output path: "
          hFlush stdout
          p <- getLine
          writeSummaryToFile p st
          putStrLn $ "Report written to " ++ p
      mainMenu st
    "0" -> do
      putStrLn "Goodbye!"
      exitSuccess
    _ -> do
      putStrLn "Invalid option."
      mainMenu st
