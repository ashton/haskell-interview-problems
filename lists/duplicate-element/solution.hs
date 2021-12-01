import qualified Data.HashMap.Strict as Map

-- simpler way, O(1) for space and O(n^2) for time
findDuplicate :: [Int] -> Maybe Int
findDuplicate (x:xs)
    | xs == [] = Nothing
    | otherwise = if (x `elem` xs)
                  then Just x
                  else findDuplicate xs

-- hashing approach, O(n) for space and time
findDuplicate' :: [Int] -> Maybe Int
findDuplicate' (x:xs) =
  Map.lookup x hash
  where hash = foldr (\x acc -> Map.insert x x acc) Map.empty xs


main :: IO ()
main = do
  putStrLn "Simplest solution for: [1, 3, 5, 7, 1]"
  let message = case findDuplicate [1, 3, 5, 7, 1] of Nothing -> "No duplicates found."
                                                      (Just x) -> "The duplicate number is: " ++ show x
    in putStrLn message

  putStrLn "Hashing solution for: [1, 3, 5, 7, 1]"
  let message = case findDuplicate' [1, 3, 5, 7, 1] of Nothing -> "No duplicates found."
                                                       (Just x) -> "The duplicate number is: " ++ show x
    in putStrLn message
