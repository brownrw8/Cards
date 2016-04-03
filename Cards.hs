module Cards (createCard
             ,showCard
             ,createDeck
             ,showDeck
             ,shuffle
             ,fShuffle
             ) where

import System.Random (getStdGen,newStdGen,randomR,StdGen)

data Suit = Spades | Clubs | Hearts | Diamonds

type Face = Int
data Card = Card Face Suit

type Deck = [Card]

faces :: [Int]
faces = [1..13]

combineDecks :: IO Deck -> IO Deck -> IO Deck
combineDecks d1 d2 = do
    md1 <- d1
    md2 <- d2
    return (md1 ++ md2)
    
fShuffle :: IO Deck -> Int -> IO Deck
fShuffle deckIO x = do
    deck <- deckIO
    shuffle deck x
    
fShowDeck deck = fmap showDeck deck

shuffle :: Deck -> Int -> IO Deck
shuffle deck x = do
    if x > 0
        then do
            g <- newStdGen
            let newDeck = mixDeck deck g
            shuffle newDeck (x-1)
        else do
            return deck
            
newShuffle x = shuffle (createDeck) x
newShuffledShort = shuffle (createDeck) 150000
newShuffledMed = shuffle (createDeck) 500000
newShuffledLong = shuffle (createDeck) 1000000
    

mixDeck :: Deck -> StdGen -> Deck
mixDeck [] g = []
mixDeck all@(c1:c2:cs) g = case f of
    ('a',g)           -> last all : init all
    ('b',g)           -> (c2 : c1 : []) ++ cs
    otherwise         -> error "Shuffle Failure."
    where f = (randomR ('a','b') g)
    

createCard :: Face -> Suit -> Card 
createCard face suit = Card face suit

createDeck :: Deck
createDeck = map (\x -> createCard x Spades) faces
          ++ map (\x -> createCard x Clubs) faces
          ++ map (\x -> createCard x Hearts) faces          
          ++ map (\x -> createCard x Diamonds) faces   
          
showDeck :: Deck -> [[Char]]
showDeck [] = []
showDeck (c:cs) = showCard c : showDeck cs
          
showCard :: Card -> [Char]
showCard (Card f s) = (showFace f) ++ " of " ++ (showSuit s)

showFace :: Face -> [Char]
showFace x = case x of  
    1   -> "Ace"
    2   -> "Two"
    3   -> "Three"
    4   -> "Four"
    5   -> "Five"
    6   -> "Six"
    7   -> "Seven"
    8   -> "Eight"
    9   -> "Nine"
    10  -> "Ten"
    11  -> "Jack"
    12  -> "Queen"
    13  -> "King"
    
showSuit :: Suit -> [Char]
showSuit x = case x of  
    Spades      -> "Spades"
    Clubs       -> "Clubs"
    Hearts      -> "Hearts"
    Diamonds    -> "Diamonds"
    
    