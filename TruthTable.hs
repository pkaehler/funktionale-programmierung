module TruthTable
where

import           Data.List (intercalate)


type TruthTable = [[Bool]]

-- | generate a truth table for n variables

truthTable :: Int -> TruthTable
truthTable 0 = [[]]
truthTable n = map (True:) t' ++ map (False:) t'
  where t' = truthTable (n - 1)

ppTruthTable :: TruthTable -> String
ppTruthTable tt
  = unlines (map ppRow tt)
  where
    ppRow r = intercalate " " (map (take 1 . show) r)

printTT :: TruthTable -> IO ()
printTT = putStrLn . ppTruthTable

-- ----------------------------------------