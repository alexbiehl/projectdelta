
module Main where

import Server (runServer)
import Core (withCoreEnv)

main :: IO ()
main = withCoreEnv runServer