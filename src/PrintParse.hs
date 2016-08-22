-- | Functions to handle parsing input and printing output in the Mastermind game.
module PrintParse
    ( parseCodeGuess
    , prompt
    , printFeedback
    , parseCodeLength
    ) where
import Types
import Text.Read (readMaybe)
import System.IO

-- | Parses a letter representing a code peg.
--   The input might be incorrect so it returns a Maybe.
parseCodePeg :: String -> Maybe CodePeg
parseCodePeg "R" = Just Red
parseCodePeg "O" = Just Orange
parseCodePeg "Y" = Just Yellow
parseCodePeg "G" = Just Green
parseCodePeg "B" = Just Blue
parseCodePeg "P" = Just Purple
parseCodePeg _   = Nothing

-- | Parses a string consisting of space-separated letters that represent the code pegs.
parseCodeGuess :: String -> Int -> Maybe Guess
parseCodeGuess guess codeLength
    -- If the length of the guess is not the same as the length of the code, we cannot have a match
    | length pegStrings /= codeLength = Nothing
    -- Similar situation as with getRandomCode: mapping parseCodePeg returns [Maybe CodePeg],
    --     but we want a Maybe [CodePeg] so we use mapM. If any of the elements is Nothing,
    --     the whole value will be Nothing, so parse errors propagate.
    --     As before, we also fmap the Guess data constructor to get a Maybe Guess.
    | otherwise = Guess <$> mapM parseCodePeg pegStrings
        -- The words function splits a string with space-separated words into a list of words
        where pegStrings = words guess

-- | Prints a prompt message and returns the user's response.
prompt :: String -> IO String
-- If buffering is turned on, the printing happens only after you press Enter - we need to turn this off
prompt message = hSetBuffering stdout NoBuffering >> putStr message >> getLine

-- | Prints the feedback.
printFeedback :: Feedback -> IO ()
printFeedback (Feedback f) = putStrLn $ "Feedback: " ++ concatMap ((++ "   ") . show) f

-- | Parses and validates the user's input for length of the code.
parseCodeLength :: String -> Maybe Int
parseCodeLength len = case readMaybe len of
    Just n -> if n `elem` [2..10] then Just n else Nothing
    Nothing -> Nothing
