-- | Write a report describing your design and strategy here.
-- idea inspired from: https://yjyao.com/res/hearts.pdf

-- This algorithm greedily selects the best card in the set of all currently playable cards with the
-- following rules:
-- 1. If the player is the first player in the round, pick the smallest card
    -- 2. Otherwise
    -- a. If the player does not need to play the same suit as the first player
        -- 1. If the player can play Q of Spade, pick Q of Spade
        -- 2. If the player can play hearts, pick the largest card of hearts
        -- 3. Otherwise, pick the largest card
-- b. Otherwise
    -- 1. if the player can pick a smaller card than the first card, pick the largest that is
    -- smaller than the first card
    -- 2. Otherwise,
    -- a. If the player is not the last player, pick the smallest card
    -- b. Otherwise, pick the largest card

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
-- Trick list is not empty, so it means someone already played. I must not renege.
playCard _ hand trick _ = (renege (suit $ fst $ last trick) hand, "")

-- | The renege function takes in a leader suit and selects the first occuring card of that suit from a list of cards.
-- Will throw an error if no cards of that suit is available. This renege is complete.
renege :: Suit -> [Card] -> Card
renege leader hand = select $ filter (\x -> suit x == leader) hand where
    select :: [Card] -> Card
    select [] = head hand -- do not have lead suit, just release the head of the hand
    select leadCard = head leadCard -- release the similar suit of the head

-- | The lead function is for the start of the trick. It always tries to choose 2 of clubs if I have it.
lead :: [Card] -> Card
lead hand = select (find (== (Card Club Two)) hand) where
    -- | Select the first card from hand if no particular card was provided. Does not satisfiy Bleeding rule.
    select :: Maybe Card -> Card
    select Nothing = filterLeads hand
    select card = fromJust card

-- bleeding rule
filterLeads :: [Card] -> Card
filterLeads hand = select $ filter (\x-> x /= Card Spade Queen) (filter(\x -> suit x /= Heart) hand) where
    select :: [Card] -> Card
    select [] = head hand -- if empty hands, do not filter, release head of hand
    select filter_hand  = head filter_hand -- if got something, do filter for the hand 


-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s


-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
------------------------------------------------------------------------------------------------------
-- If player is the first player in the round, need to pick the smallest card

-- renege :: Suit -> [Card] -> Card
-- renege leader hand = head $ filter (\x -> suit x == leader) hand

-- lead :: [Card] -> Card
-- lead hand = select (find (== (Card Club Two)) hand) where
--     -- | Select the first card from hand if no particular card was provided. Does not satisfiy Bleeding rule.
--     select :: Maybe Card -> Card
--     select Nothing = head hand -- If don't have this Club Two, just take the first card in the list
--     select card = fromJust card -- If you have the Club Two card, use fromJust to extract the card
