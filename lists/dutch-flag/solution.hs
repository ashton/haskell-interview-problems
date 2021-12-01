import Data.List (partition)
-- sort array of 0's and 1's

-- simplest solution O(n) in time and O(n) in space
sortBinaryList :: [Int] -> [Int]
sortBinaryList xs =
  let zeroList = filter (\x -> x == 0) xs
    in zeroList ++ replicate (length xs - length zeroList) 1

-- count approach solution O(n) in time and O(1) in space
sortBinaryListCount :: [Int] -> [Int]
sortBinaryListCount xs = let counters = foldr (\x (zeroes, ones) -> if x == 0
                                                                   then (zeroes+1, ones)
                                                                   else (zeroes, ones+1)) (0,0) xs
                           in replicate (fst counters) 0 ++ replicate (snd counters) 1

sortBinaryListPartition :: [Int] -> [Int]
sortBinaryListPartition xs =
  zeroes ++ ones
  where (zeroes,ones) = partition (==0) xs

main :: IO ()
main = do
  putStrLn "Ordering array [1, 1, 0, 1, 1, 0, 0, 0, 1, 0]"
  putStrLn $ show $ sortBinaryList [1, 1, 0, 1, 1, 0, 0, 0, 1, 0]

  putStrLn "Ordering array [1, 1, 0, 1, 1, 0, 0, 0, 1, 0] with count approach"
  putStrLn $ show $ sortBinaryListCount [1, 1, 0, 1, 1, 0, 0, 0, 1, 0]

  putStrLn "Ordering array [1, 1, 0, 1, 1, 0, 0, 0, 1, 0] with partition approach"
  putStrLn $ show $ sortBinaryListPartition [1, 1, 0, 1, 1, 0, 0, 0, 1, 0]
