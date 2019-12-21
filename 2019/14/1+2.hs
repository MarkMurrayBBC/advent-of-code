import Data.List.Split (splitOn)
-- import Data.List (List)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (traceShow, traceShowId)

type Pair = (String, Int)
type Reaction = (Pair, [Pair])
type Store = Map String Int

storeValue :: Store -> String -> Int
storeValue store name = case Map.lookup name store of
  Just amount -> amount
  Nothing -> 0

storeTake :: Store -> String -> Int -> (Store, Int)
storeTake store name amount =
  let
    inStock = storeValue store name
    newStockLevel = inStock - amount
    newStore = Map.insert name (max 0 newStockLevel) store
    remaining = if newStockLevel < 0 then -newStockLevel else 0
  in (newStore, remaining)


storeAdd :: Store -> String -> Int -> Store
storeAdd store name amount = Map.insert name ((storeValue store name) + amount) store


-- |
-- >>> minMultiplier 1 1
-- (1,0)
-- >>> minMultiplier 8 2
-- (4,0)
-- >>> minMultiplier 8 3
-- (3,1)
-- >>> minMultiplier 2 2
-- (1,0)
minMultiplier :: Int -> Int -> (Int, Int)
minMultiplier required multiples =
  let
    rem = required `mod` multiples
    ex = if rem == 0 then 0 else 1
    m = (required `div` multiples) + ex
  in (m, (m * multiples) `mod` required)

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


oreRequiredFor :: [Reaction] -> Pair -> Store -> (Store, Int)
oreRequiredFor reactions ("ORE", amount) store = (store, amount)
oreRequiredFor reactions (name, amount) store =
  let
    ((_, count), ingredients) = head $ filter (((==) name) . fst . fst) reactions
    (store1, remainingAmount) = storeTake store name amount
    (m, leftOver) = minMultiplier remainingAmount count
    newStore = storeAdd store1 name leftOver
  -- in sum $ map (oreRequiredFor reactions) $ map (\(n, q) -> (n, q * multiplier)) ingredients
  in List.foldl' (\(s,t) (iname, iamount) ->
    let
      (newS, a) = oreRequiredFor reactions (traceShowId (iname, iamount * m)) s
    in (newS, t + a)) (newStore, 0) ingredients

main = do
  input <- getContents
  let reactions = parseInput input
  print $ "Part 1: " ++ (show $ oreRequiredFor reactions ("FUEL", 1) mempty)
