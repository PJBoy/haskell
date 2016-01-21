module Cards where
    import Data.List
    import System.Random
    import Data.Ord
    -- Show converts these enumations into strings of their name. Eq/Ord: (in)equality. Enum: Use in a list
    data Suit = Club | Diamond | Heart | Spade deriving (Show, Read, Eq, Ord, Enum);
    data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Read, Eq, Ord, Enum);
    data Card = Card Rank Suit deriving (Show, Read, Eq, Ord);

    -- Name of card: "<Ace> of <Spade>s"
    display :: Card -> String
    display (Card r s) = show r ++ " of " ++ show s ++ "s"

    --                    /----x----\ /-----y-----\
    -- List of all cards: <Ace..King>,<Club..Spade>
    pack :: [Card]
    pack = [(Card r s) | r <- [Ace .. King], s <- [Club .. Spade]]

    -- Shuffles a list of cards
    shuffle :: Int -> [a] -> [a]
    shuffle seed cards = map fst (sortBy (\(_, a) (_, b) -> compare a b) (zip cards (randoms (mkStdGen seed) :: [Int])))

    -- Distributes a deck to a number of hands 'transpose'ly, with later hands' cards <= earlier hands' cards
    deal :: Int -> [a] -> [[a]]
    deal hands cards = transpose ([take hands (drop (i*hands) cards) | i <- [0..div (length cards) hands]])