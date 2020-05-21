main :: IO ()
main =
  print ""

data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum, Eq, Ord, Show)

class (Eq a, Ord a, Enum a) => Die a where
  isMax :: a -> Bool

instance Die FiveSidedDie where
  isMax S5 = True
  isMax _ = False



data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | White
  | Brown deriving (Show,Eq)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) White c = c
  (<>) c White = c
  (<>) a b | a == b = a
           | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
           | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
           | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
           | otherwise = Brown

instance Monoid Color where
  mempty = White
  mappend = (<>)


newtype Events = Events [String]

instance Semigroup Events where
  (<>) (Events []) (Events []) = Events []
  (<>) e1 (Events []) = e1
  (<>) (Events []) e2 = e2

instance Monoid Events where
  mempty = Events []
  mappend (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
    where combiner = (\x y -> mconcat [x,"-",y])

newtype Probs = Probs [Double]

instance Semigroup Probs where
  (<>) (Probs []) (Probs []) = Probs []
  (<>) p1 (Probs []) = p1
  (<>) (Probs []) p2 = p2

instance Monoid Probs where
  mempty = Probs []
  mappend (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

data PTable = PTable Events Probs

instance Semigroup PTable where
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = mappend e1 e2
          newProbs = mappend p1 p2

instance Monoid PTable where
  mempty = PTable mempty mempty
  mappend = (<>)

createPTable :: Events -> Probs -> PTable
createPTable events (Probs probs) = PTable events (Probs normalizedProbs)
  where totalProbs = sum probs
        normalizedProbs = map (\x -> x/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|", show prob,"\n"]

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where pairs = zipWith showPair events probs


cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where nToAdd = length l2
        repeatedL1 = map (take nToAdd . repeat) l1
        newL1 = mconcat repeatedL1
        cycledL2 = cycle l2
