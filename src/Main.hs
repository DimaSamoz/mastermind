module Main where

import Types
import CompActions
import PrintParse
import Control.Monad (forever, when)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    putStrLn "Welcome to Mastermind!"
    codeLength <- parseCodeLength <$> prompt "How long should the code be? " :: IO (Maybe Int)
    case codeLength of
        Just len -> playBreak len
        Nothing -> putStrLn "The code length should be a number between 2 and 10. Please try again." >> main

playBreak :: Int -> IO ()
playBreak codeLength = do
    code <- getRandomCode codeLength
    putStrLn $ "\nThe computer has created a code of length " ++ show codeLength ++ "."
    putStrLn $ "Guess " ++ show codeLength ++ " colours from Red (R), Orange (O), \nYellow (Y), Green (G), Blue (B), or Purple (P).\n"
    forever $ do
        guessString <- prompt "Guess: "
        case parseCodeGuess guessString codeLength of
            Just guess -> do
                let feedback = getFeedback code guess
                printFeedback feedback
                when (correctGuess code guess) $ do
                    putStrLn "Correct! Well done!"
                    exitSuccess
            Nothing -> do
                putStrLn "Couldn't recognise your input, please try again."
                return ()
