import Data.List.Split (splitOn)

type Pair = (String, Int)
type Reaction = (Pair, [Pair])

parseInput :: String -> [Reaction]
parseInput input = map parseLine $ lines input
  where
    parsePair :: String -> Pair
    parsePair pair =
      let [a,b] = splitOn " " pair
      in  (b, read a)
    parseLine :: String -> Reaction
    parseLine line =
      let [ing, res] = splitOn " => " line
      in  (parsePair res, map parsePair $ splitOn ", " ing)


main = do
  input <- getContents
  let reactions = parseInput input
  print $ reactions
