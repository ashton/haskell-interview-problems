import qualified Data.HashMap.Strict as Map
import qualified Data.List as List

countElements :: [Int] -> Map.HashMap Int Int
countElements xs = foldr addOrIncrement Map.empty xs
  where addOrIncrement x m = Map.insertWith (+) x 1 m

sortBasedOnOtherArray :: [Int] -> [Int] -> [Int]
sortBasedOnOtherArray xs order =
  let counters = countElements xs
    in concat $ map (\x -> replicate (Map.findWithDefault 0 x counters) x) order

customCompare :: [Int] -> Int -> Int -> Ordering
customCompare order a b =
  let firstIndex = List.elemIndex a order
      secondIndex = List.elemIndex b order
    in case (firstIndex, secondIndex) of
         (Nothing, Nothing) -> a `compare` b -- none in the order, compare numerically
         (Nothing, Just _) -> GT
         (Just _, Nothing) -> LT
         (Just x, Just y) -> x `compare` y -- both in the order compare the indexes

sortBasedOnOtherArray' :: [Int] -> [Int] -> [Int]
sortBasedOnOtherArray' xs order = List.sortBy sortFn xs
  where sortFn = customCompare order


main :: IO ()
main = do
  putStrLn "Sort [2, 4, 5, 2, 5, 9, 8] based on [9, 2, 5, 4, 8]"
  putStrLn . show $ sortBasedOnOtherArray [2, 4, 5, 2, 5, 9, 8] [9, 2, 5, 4, 8]

  putStrLn "Sort [2, 4, 5, 2, 5, 9, 8] based on [9, 2, 5, 4, 8] using custom comparison"
  putStrLn . show $ sortBasedOnOtherArray' [2, 4, 5, 2, 5, 9, 8] [9, 2, 5, 4, 8]
