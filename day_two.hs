module Solution where
import qualified Data.Text as      Text
import qualified Data.Text.IO as   Text
import qualified Data.Text.Read as Text
import Data.Either


all_increasing (x:y:xs) = if (x < y) then all_increasing (y:xs) else False
all_increasing (x:[]) = True
all_increasing [] = True 

all_decreasing (x:y:xs) = if (x > y) then all_decreasing (y:xs) else False
all_decreasing (x:[]) = True
all_decreasing [] = True

safe_changes x y = ((abs (x - y)) <= 3) && ((abs (x - y)) >= 1)

all_safe (x:y:xs) = if (safe_changes x y) then all_safe (y:xs) else False
all_safe (x:[]) = True
all_safe [] = True

safe_level x = ((all_increasing x) || (all_decreasing x)) && (all_safe x)

safety_report xs = foldr go 0 xs
  where go x y = if (safe_level x) then y+1 else y

main = do
  input <- fmap Text.lines $ Text.readFile "./input.txt"
  let reports = fmap (fmap fst) $ fmap rights $ fmap (fmap (Text.decimal)) $ fmap Text.words input
  print (safety_report reports)

