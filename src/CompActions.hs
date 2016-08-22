module CompActions
    ( getRandomCode
    , getFeedback
    , correctGuess
    ) where
import Types
import System.Random (randomRIO)
import Control.Applicative ((<$>))
import Data.List (intersect, nub)

-- | Returns a random code peg.
getRandomCodePeg :: IO CodePeg
getRandomCodePeg =
    -- Since allPegs derives Enum, we can get all the colors with the range syntax
    let allPegs = [Red ..]
    in do
        -- Gets a random integer in the supplied range, wrapped in IO inherited from randomRIO
        randomIndex <- randomRIO (0, length allPegs - 1)
        -- Returns the peg at the specified random index
        return $ allPegs !! randomIndex  --

-- | Returns a random code of the specified length.
getRandomCode :: Int -> IO Code
-- randomCodePeg returns an IO CodePeg and a normal map would give us a [IO CodePeg],
--     but we want instead an IO [CodePeg] which is achieved with mapM. We also want
--     to wrap the list in a Code constructor so we use fmap to put it under the IO.
getRandomCode len = Code <$> mapM (const getRandomCodePeg) [1..len]

-- | Compares a guess to the secret code and returns the feedback encoded with black and white key pegs.
getFeedback :: Code -> Guess -> Feedback
getFeedback (Code c) (Guess g) =
    -- The order of the key pegs doesn't matter so we first give the black pegs and then the white pegs.
    Feedback $ replicate numCorrectPlace Black ++ replicate numCorrectColourButNotPlace White
    where
        -- The number of code pegs that match in colour and position: straigtforward zipping with equality
        numCorrectPlace = length . filter id $ zipWith (==) c g
        -- The number of code pegs that match in colour: we remove the duplicate colours from the
        --     code and guess and return the length of their intersection.
        numCorrectColour = length $ intersect (nub c) (nub g)
        -- The number of pegs that match in colour but have the wrong position: in case of duplicates
        --     the there can be more place matches than colour matches so we truncate the subtraction to 0
        numCorrectColourButNotPlace = if diff < 0 then 0 else diff
            where diff = numCorrectColour - numCorrectPlace

-- | Whether the guess matches the code.
correctGuess :: Code -> Guess -> Bool
correctGuess (Code c) (Guess g) = c == g
