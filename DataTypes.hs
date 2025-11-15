module DataTypes
  ( StudentId
  , Name
  , Mark
  , Student(..)
  , Grade(..)
  , Summary(..)
  , parseStudentLine
  ) where

import Utils (splitOn, trim)

type StudentId = String
type Name = String
type Mark = Int

data Student = Student
  { sid   :: StudentId
  , name  :: Name
  , marks :: [Mark]
  } deriving (Show, Eq)

data Grade = A | B | C | D | F
  deriving (Show, Eq, Ord)

data Summary = Summary
  { sAvg   :: Double
  , sMax   :: Int
  , sMin   :: Int
  , sGrade :: Grade
  } deriving (Show, Eq)

-- Parse a CSV line with aggregated marks:
-- Expect format: StudentID,Name,mark1;mark2;mark3
-- Example: S001,Ayesha Perera,78;82;91
parseStudentLine :: String -> Maybe Student
parseStudentLine line =
  case splitOn ',' line of
    (i:n:m:[]) ->
      let i' = trim i
          n' = trim n
          markStrs = splitOn ';' m
          parseIntSafe s =
            case reads (trim s) :: [(Int, String)] of
              [(v, "")] -> Just v
              _           -> Nothing
      in case traverse parseIntSafe markStrs of
           Just ms -> Just $ Student i' n' ms
           Nothing -> Nothing
    _ -> Nothing
