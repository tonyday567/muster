module Main where

import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-isrc",
      "-XGHC2021",
      "-XOverloadedStrings",
      "-XDerivingStrategies",
      "-XGeneralisedNewtypeDeriving",
      "-XBlockArguments",
      "src/Muster/Api/Types.hs",
      "src/Muster/Api/Bus.hs",
      "src/Muster/Api/Participant.hs",
      "src/Muster/Api/Agent.hs",
      "src/Muster/Api/Orchestrator.hs"
    ]
