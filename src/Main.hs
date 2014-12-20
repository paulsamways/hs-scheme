module Main where

import Scheme
import Control.Monad
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          input <- getInputLine "> "
          case input of
           Nothing -> loop
           Just ":q" -> outputStrLn "Exiting..."
           Just s  -> do
             value <- return $ liftM show $ readExpr s >>= eval
             outputStrLn $ extractValue $ trapError value
             loop
