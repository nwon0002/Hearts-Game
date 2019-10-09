-- | Write a report describing your design and strategy here.
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

-- Nicholas: You need to read Types.hs and Cards.hs at minimum to start doing things.
-- Play.hs contains the sequence of play for every round, but it's not necessary to fully understand it.
-- You only need to know how the game works, and you just need to play a single card, everything else is automated.
-- The problem is choosing the right card to play. This is what you'll need to solve.

playCard :: PlayFunc
-- Empty trick list means beginning of trick. I have to lead.
playCard _ hand [] _ = (lead hand, "")
-- Trick list is not empty, so it means someone already played. I must not renege.
playCard _ hand trick _ = (renege (suit $ fst $ last trick) hand, "")

-- | The renege function takes in a leader suit and selects the first occuring card of that suit from a list of cards.
-- Will throw an error if no cards of that suit is available. This renege is incomplete.
renege :: Suit -> [Card] -> Card
renege leader hand = head $ filter (\x -> suit x == leader) hand

-- | The lead function is for the start of the trick. It always tries to choose 2 of clubs if I have it.
lead :: [Card] -> Card
lead hand = select (find (== (Card Club Two)) hand) where
    -- | Select the first card from hand if no particular card was provided. Does not satisfiy Bleeding rule.
    select :: Maybe Card -> Card
    select Nothing = head hand
    select card = fromJust card

-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s


-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
