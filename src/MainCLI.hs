module Main where

import Core (initGame)
import Interface.CLI (startCLI)

main :: IO ()
main = startCLI initGame
