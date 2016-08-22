-- | The types used in the Mastermind game.
module Types
    ( CodePeg (..)
    , KeyPeg (..)
    , CodePegs (..)
    , Code (..)
    , Guess (..)
    , Feedback (..)
    ) where

-- | The colors from which the code is constructed.
data CodePeg = Red | Orange | Yellow | Green | Blue | Purple deriving (Eq, Show, Enum)

-- | The colors which are used to indicate the correctness of the guess.
data KeyPeg = Black | White deriving (Eq, Show)

-- | A sequence of code pegs.
type CodePegs = [CodePeg]

-- | A code made up of a sequence of code pegs.
newtype Code = Code CodePegs deriving (Eq, Show)

-- | A guess of the code made up of a sequence of code pegs.
newtype Guess = Guess CodePegs deriving (Eq, Show)

-- | A feedback for a guess made up of some number of key pegs.
newtype Feedback = Feedback [KeyPeg] deriving (Eq, Show)
