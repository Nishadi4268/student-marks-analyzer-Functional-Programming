module Processing
  ( studentAverage
  , gradeFromAverage
  , studentSummary
  , cohortAverage
  , topStudents
  , gradeDistribution
  , filterByThreshold
  , studentSummariesPar
  ) where

import DataTypes
import Utils (safeDiv)
import Data.List (sortBy, foldl')
-- Optional: to enable true parallel evaluation of summaries, install the `parallel`
-- package and replace the implementation of `studentSummariesPar` below with a
-- strategies-based version using `withStrategy (parList rdeepseq)`. We keep a
-- sequential fallback so the project loads without external packages.

-- Compute student average (0 if no marks)
studentAverage :: Student -> Double
studentAverage (Student _ _ ms) =
  if null ms then 0 else safeDiv (fromIntegral (sum ms)) (fromIntegral (length ms))

-- Convert numeric average to grade
-- thresholds: A >= 80, B >= 70, C >= 60, D >= 50, else F
gradeFromAverage :: Double -> Grade
gradeFromAverage avg
  | avg >= 80 = A
  | avg >= 70 = B
  | avg >= 60 = C
  | avg >= 50 = D
  | otherwise = F

-- Build a summary for a student
studentSummary :: Student -> Summary
studentSummary s@(Student _ _ ms) =
  let avg = studentAverage s
      mx = if null ms then 0 else maximum ms
      mn = if null ms then 0 else minimum ms
      g  = gradeFromAverage avg
  in Summary avg mx mn g

-- Cohort average across students (average of student averages; empty list -> 0)
cohortAverage :: [Student] -> Double
cohortAverage [] = 0
cohortAverage sts =
  let avgs = map studentAverage sts
  in safeDiv (sum avgs) (fromIntegral $ length avgs)

-- Top N students by average. Returns list of (Student, avg) sorted desc.
topStudents :: Int -> [Student] -> [(Student, Double)]
topStudents n sts =
  take n $ sortBy (\(_, a) (_, b) -> compare b a) $
    map (\s -> (s, studentAverage s)) sts

-- Grade distribution: count of each grade present
gradeDistribution :: [Student] -> [(Grade, Int)]
gradeDistribution sts =
  foldl' addGrade [(A,0),(B,0),(C,0),(D,0),(F,0)] (map (sGrade . studentSummary) sts)
  where
    addGrade acc g = map (\(gr, c) -> if gr == g then (gr, c+1) else (gr, c)) acc

-- Filter students whose average >= threshold
filterByThreshold :: Double -> [Student] -> [Student]
filterByThreshold t = filter (\s -> studentAverage s >= t)

-- Parallel computation of student summaries using Strategies
studentSummariesPar :: [Student] -> [Summary]
studentSummariesPar sts = map studentSummary sts
