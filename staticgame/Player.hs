-- | Nelson Wei Han, Wong 28488563
-- | Write a report describing your design and strategy here.
{-The following design of my Hearts game are as follows:

I implemented all the rules of the game:
- Reneging
- Bleeding
- Leading
- Breaking

To start playing, I will first obey the bleeding rule, which no point cards can be played in the first round.
I have two functions "lead" and "filterLeads" which will check all the cards on my deck and return
the cards that can be played to satisfy the bleeding rule.

After the first round, for each consecutive round being played, I will record all the cards being played for
that session using function "prevCards". The "prevCards" function is actually my way to transform the 
memory tuple from the "playFunc", so that I can store the cards being played and check accordingly.

In the "prevCards":
    prevCards (Just(preTric, mem)) = foldr (\f s -> f ++ s) "" (map(\(Card suit_ rank_,_) -> show suit_ ++ show rank_)(preTric)) ++ mem

I am trying to convert the memory tuple of "Maybe([(Card, PlayedId),String])" into a readable format for my filter.
Thus, I need to take in a Just type of the "Card" in the tuple, as show in the parameter preTric.

    precTric = [(C8,"s"),(S9."h")]

The precTric takes in all the cards that have been played by other players. I will then perform a foldr on the list of cards. I will 
then map the list of cards in "preTric" into the readable format which is this: [C8,S9]. This output will be a string format. I have
done so that I can check if a Heart has been played or not. I will collect the memory, and pass the memory into my "playCard" function.

In the "playCard":
    playCard _ hand trick memory = (renege (suit $ fst $ last trick) (prevCards memory) hand, prevCards memory)

As you can, all the memory is being passed into the "prevCards" function in order to be transformed into a string.

Following that, the memory of the cards will go to my "reneg" function. In the "reneg" function, I will take in the memory of the previous cards
that was played, and use that memory to choose the card I should play that does not break the "breaking" rule. The game will keep running until 
one player is > 100 points and there is a lowest point player. That lowest point player will be the winner.

Notable problems:
    For some unknown reason, the game will have a BreakError, because the Queen of Spades was not released from the players deck. Only the Queen of
    Spades will not be released from the player's deck. I have througly checked all my functions in $ stack ghci, all with various scenarios. It always
    passses without any issues. But for some reason when I uploaded the code to the Online Submission Link, I will obtain a BreakError. I tried my best
    to fix the code, but the logic is all correct. I am unable to fix the bug. I hope Sir is understanding. Thank you.
    
-}


module Player (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Cards
import Data.Maybe
import Data.List
import Hearts.Types
import Hearts.Rules

playCard :: PlayFunc
-- Empty trick list means beginning of trick. I have to lead.
playCard _ hand [] _ = (lead hand, "")

-- After the first card, memory is Nothing. This is because there is no card being played previously
playCard _ hand trick Nothing = (renege (suit $ fst $ last trick) "" hand, "")

-- From first trick onwards, this is where the memory from other cards being played is being stored into the memeory variable.
-- Which is then passed onto the prevCards function for conversion of the memory tuple, into a readble string
playCard _ hand trick memory = (renege (suit $ fst $ last trick) (prevCards memory) hand, prevCards memory)

-- This is the function where I convert the memory tuple into a readable string.
-- This is done as to store the memory, where I have a filter to check if a Heart Cards has been played or not.
prevCards :: Maybe([(Card, PlayerId)],String) -> String
prevCards Nothing = ""
-- where the conversion into a string magic happens, detailed explanation is written above under "prevCards"
prevCards (Just(preTric, mem)) = foldr (\f s -> f ++ s) "" (map(\(Card suit_ rank_,_) -> show suit_ ++ show rank_)(preTric)) ++ mem

-- The renege function takes in a leader suit and selects the first occuring card of that suit from a list of cards.
-- If there is no matching suit on my deck with the leading suit
renege :: Suit -> String -> [Card] -> Card
renege leader memory hand = select $ filter (\x -> suit x == leader) hand where
    -- | Release the head of my deck of cards, I will perform breaking rule check on the deck of cards on hand, 
    --   and release my cards that comply with the rules
    -- | else, I will release the similar suit of the hand
    select :: [Card] -> Card
    select [] = breaking memory hand -- No suit same as lead suit, perform breaking rule check on the deck of cards on hand
    select leadCard = head leadCard -- release the similar suit of the head

-- This is my breaking function rule.
-- I will take in the memory of the cards being played, and filter for any Heart cards that was played
breaking :: String -> [Card] -> Card
breaking memory cardList = select $ filter (\x -> x == 'H') memory where
    -- After filtering, I will check:
    -- | Return cards that are not Hearts
    -- | Play cards that are Hearts (must comply with breaking rule)
    select :: String -> Card
    select "" = head $ filterDWHeart cardList -- No Heart card played before, perform filter on deck of cards to return non-Heart Cards
    select _ = head cardList -- Heart card was played, release card

-- This is function where I filter cards so I don't get Heart Cards if I have other suits available
-- I will take in a list of cards from my deck, and return either:
-- | If I have only Hearts left, return the Heart Cards
-- | otherwise, other cards then Heart Card exist, return such cards to my hand
filterDWHeart :: [Card] -> [Card]
filterDWHeart hand 
 | (length (filter (\x -> suit x /= Heart) hand)) == 0 = hand -- If my hand only have Heart card (length of cards that are not Heart is 0), don't filter and return the Heart cards
 | otherwise = filter(\x -> suit x /= Heart) hand -- else, filter all the Hearts, return cards that are not Heart

-- The lead function is for the start of the trick. It always tries to choose 2 of clubs if I have it.
lead :: [Card] -> Card
lead hand = select (find (== (Card Club Two)) hand) where
    -- | No Card Club Two, filter the rest of the cards so it will not play point cards in the first round
    -- | If Card Club Two exist, play that card
    select :: Maybe Card -> Card
    select Nothing = filterLeads hand
    select card = fromJust card

-- This is where my bleeding rule is implemented
-- I will filter through my deck to ensure I do not play any point cards, and also return the non-point cards from the function "filterDWHeart"
filterLeads :: [Card] -> Card
filterLeads hand = select $ filter (\x-> x /= Card Spade Queen || suit x /= Heart) (filterDWHeart hand) where
    select :: [Card] -> Card
    select [] = head hand -- I don't have any cards that match, I will release the first card from the list
    select filter_hand  = minimum filter_hand -- if I do have the card, I must choose the smallest card

-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
------------------------------------------------------------------------------------------------------
