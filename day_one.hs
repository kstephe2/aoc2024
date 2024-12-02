module Solution where
import qualified Data.Text    as   Text
import qualified Data.Text.IO as   Text
import qualified Data.Text.Read as Text
import Data.Either
import Data.List

combine_lists xs ys = zipWith go xs ys
  where go x y = abs $ x - y

build_lists d = go d [] [] where
  go (x:xs) l ll = go xs ((head x) : l) ((head $ tail x) : ll)
  go [] l ll = (l, ll)

build (x:xs) = (fst x) : (build xs)
build []     = []

main = do
  numbers <- fmap (fmap Text.words) $ fmap Text.lines (Text.readFile "input.txt")
  let ls = (build_lists numbers)
  let l1 = build (rights (fmap Text.decimal (fst ls)))
  let l2 = build (rights (fmap Text.decimal (snd ls)))
  let combined = combine_lists (sort l1) (sort l2)
  print (sum combined)
